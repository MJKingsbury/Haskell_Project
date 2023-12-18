-- | This is the main module of the Lichess Database Application.
-- It handles command line arguments and coordinates actions like model initialization,
-- player data fetching and processing, and database interaction.

module Main (main) where

import Fetch
import Types
import Model
import Parse
import Database
import Database.SQLite.Simple
import System.Environment

-- | The 'main' function serves as the entry point of the application.
-- It processes command line arguments and executes from a set of commands.
main :: IO ()
main =  do
    conn <- initialise
    command <- getArgs
    case command of
        -- | Initialises the machine learning model on a dataset
        ["initialiseModel"] -> do
            putStrLn "Initialising Classification Model..."
            let usernames = ["perlez", "ranand1653", "birdlarry", "buonevervaz", "darthmccartney"]
            let parseGames :: Int -> Connection -> [String] -> IO ()
                parseGames _ _ [] = do
                    return ()
                parseGames a conn (name:names) = do
                    parseGame a conn name
                    parseGames (a + numgamesfetch) conn names
            parseGames 0 conn usernames
            (weight, bias) <- modelInitialise conn
            putStrLn "Use Command \'predict\' with two Players to perform predictions"
            putStrLn "Player 1 will take the White Pieces, Player 2 will take the Black Pieces"
            writeFile "weights.txt" (show weight ++ "\n" ++ show bias)
        -- | Takes two players and uses the classifier to predict the winner
        ["predict", p1, p2] -> do
            check <- checkModelInit conn
            case check of
                Nothing -> do
                    putStrLn "Please initialise model before performing predictions"
                    putStrLn "Use command \'initialiseModel\'"
                    syntaxError
                _ -> do
                    player1 <- getPlayer conn p1
                    player2 <- getPlayer conn p2
                    case (player1, player2) of
                        (Nothing, _) -> putStrLn $ show NoPlayerFException
                        (_, Nothing) -> putStrLn $ show NoPlayerFException
                        (Just player1, Just player2) -> do
                            weights <- readFile "weights.txt"
                            let [w, b] = map read $ lines weights
                            avg1 <- getAverageRating conn player1
                            avg2 <- getAverageRating conn player2
                            let prediction = comparePlayers avg1 avg2 w b
                            case prediction of
                                1.0 -> putStrLn "Model Predicts Player 1 to Win!"
                                0.0 -> putStrLn "Model Predicts Player 2 to Win!"
                                _   -> putStrLn $ show PredictionError
        -- | Parses a player retrieved through the API and adds to the database
        ["addPlayer", p] -> do
            parsePlayer conn p
        -- | Retrieves and joins the information from the 'players' and 'gametypes'
        -- tables and displays the information to the user
        ["getPlayerInfo", p] -> do
            playerInfo <- getPlayerInfo conn p
            case playerInfo of
                Nothing -> putStrLn $ show NoPlayerFException
                Just player -> do
                    putStrLn "Player Information:"
                    putStrLn $ "Username: " ++ username_ player
                    putStrLn $ "URL: " ++ url_ player
                    putStrLn $ "Total Games Played: " ++ (show (games_ player))
                    putStrLn $ "Total Wins: " ++ (show (wins_ player))
                    putStrLn $ "Total Draws: " ++ (show (draws_ player))
                    putStrLn $ "Total Losses: " ++ (show (losses_ player))
                    putStrLn "Ratings..."
                    putStrLn $ info_ player
        -- | Deletes a player and their ratings from the database
        ["deletePlayer", p] -> do
            deletion <- deletePlayer conn p
            case deletion of
                Nothing -> putStrLn $ show NoPlayerFException
                Just player -> putStrLn $ username player ++ " has been sucessfully deleted."
        -- | Exports the database to Json
        ["export"] -> do
            export <- exportToJson conn
            case export of
                Left exc -> putStrLn $ show exc
                Right str -> putStrLn str
        -- | Handles user passing in unsupported commands
        _ -> syntaxError
    close conn

-- | Handles syntax error display for the command line interface.
syntaxError :: IO ()
syntaxError = do 
    putStrLn "Lichess Database Application!"
    putStrLn "Usage: Command [args]"
    putStrLn "Available Commands:"
    putStrLn "initialiseModel -> initialise the machine learning model"
    putStrLn "predict [p1, p2] -> predict a winner between two players in the database"
    putStrLn "addPlayer [p] -> add a new player to the database"
    putStrLn "getPlayerInfo [p] -> retrieve a player's lichess information from the database"
    putStrLn "deletePlayer [p] -> delete a player from the database"
    