-- | This module is responsible for parsing game and player data from JSON,
-- and interacting with the database to store and update this information.

module Parse (
    parseGame,
    parsePlayer
) where

import Fetch
import Types
import Control.Exception (throwIO)
import Database
import Database.SQLite.Simple 
import Data.Char (isDigit)

-- | Parses game data from a JSON string and updates the database.
-- Throws 'InitModelException' if the JSON indicates an error.
--
-- @param idx Id value used for entry into the Games table.
-- @param conn The connection to the database.
-- @param uname The username of the player whose games are to be parsed.
parseGame :: Int -> Connection -> String -> IO ()
parseGame idx conn uname = do
    json <- (fetchGames uname)
    if json == "{\"error\":\"Not found\"}" then do
        _ <- throwIO InitModelException
        return ()
    else do
        let isWhiteRating line = take 9 line == "[WhiteElo"
        let isBlackRating line = take 9 line == "[BlackElo"
        let result line = take 7 line == "[Result"
        let whiteRating :: [Double]
            whiteRating = map read $ map (filter isDigit) $ filter isWhiteRating $ lines json
        let blackRating :: [Double]
            blackRating = map read $ map (filter isDigit) $ filter isBlackRating $ lines json
        let winner :: [Double]
            winner = map (\x -> if x > 1.1 then 1.0 else 0.0) $ map read $ map (filter isDigit) $ filter result $ lines json
        let ratingCalc :: [Double] -> [Double] -> [Double]
            ratingCalc [] [] = []
            ratingCalc (x:xs) (y:ys) = (x - y) : ratingCalc xs ys
        let ratingDiff = ratingCalc whiteRating blackRating
        let parseToGamedata :: Int -> [Double] -> [Double] -> [Gamedata]
            parseToGamedata _ [] [] = []
            parseToGamedata a (x:xs) (y:ys) = Gamedata a x y : parseToGamedata (a + 1) xs ys
        let gs = parseToGamedata idx ratingDiff winner
        _ <- addOrGetAllGames conn gs
        return ()

-- | Helper function to drop characters from a string until a given character is encountered x times.
--
-- @param x The number of times to drop characters until the specified character is encountered.
-- @param c The character to drop until.
-- @param string The string to be processed.
dropTimes :: Int -> Char -> String -> String
dropTimes 0 _ string = string
dropTimes x c string = dropTimes (x - 1) c (tail . dropWhile (/= c) $ string)

-- | Determines if a rating is provisional based on a string from the JSON.
--
-- @param string The string containing rating information.
isProv :: String -> Bool
isProv string = (take 3 $ dropTimes 2 'p' string) == "rov"

-- | Parses player information from a JSON string and updates the database.
-- Throws 'NoPlayerFException' if the JSON indicates an error.
--
-- @param conn The connection to the database.
-- @param uname The username of the player to parse.
parsePlayer :: Connection -> String -> IO ()
parsePlayer conn uname = do
    json <- (fetchPlayer uname)
    if json == "{\"error\":\"Not found\"}" then do
        _ <- throwIO NoPlayerFException
        return ()
    else do
        let getId = takeWhile (/= '\"') . drop 5 . dropWhile (/= 'i') $ json
        let getUrl = "www.lichess.org/@/" ++ getId
        let getGames x = read $ filter isDigit $ takeWhile (/= ',') $ drop 2 . dropTimes x 'H' $ dropTimes 13 '{' json :: Int
        let getDraw = getGames 1
        let getLoss = getGames 2
        let getWins = getGames 3
        let numAllGames = getDraw + getLoss + getWins
        let parseToPlayer :: String -> String -> Int -> Int -> Int -> Int -> Player
            parseToPlayer i strurl gs ws ds ls = Player i strurl gs ws ds ls
        let player = parseToPlayer getId getUrl numAllGames getWins getDraw getLoss
        _ <- addOrGetPlayer conn player
        parseGametype conn uname
        return ()

-- | Parses gametype rating entires from a JSON string and updates the database.
-- Throws 'NoPlayerFException' if the JSON indicates an error.
--
-- @param conn The connection to the database.
-- @param uname The username of the player whose gametype ratings are to be parsed.        
parseGametype :: Connection -> String -> IO()
parseGametype conn uname = do
    json <- (fetchPlayer uname)
    if json == "{\"error\":\"Not found\"}" then do
        _ <- throwIO NoPlayerFException
        return ()
    else do
        let getId = takeWhile (/= '\"') . drop 5 . dropWhile (/= 'i') $ json
        let strBlitz = drop 10 . dropWhile (/= ',') $ dropTimes 4 '{' json
        let strBullet = drop 10 . dropWhile (/= ',') $ dropTimes 6 '{' json
        let strClassical = drop 10 . dropWhile (/= ',') $ dropTimes 10 '{' json
        let strRapid = drop 10 . dropWhile (/= ',') $ dropTimes 11 '{' json
        let getBlitz = read $ filter isDigit $ take 4 strBlitz :: Int
        let getBullet = read $ filter isDigit $ take 4 strBullet :: Int
        let getClassical = read $ filter isDigit $ take 4 strClassical :: Int
        let getRapid = read $ filter isDigit $ take 4 strRapid :: Int
        let isPBlitz = isProv strBlitz
        let isPBullet = isProv strBullet
        let isPClassical = isProv strClassical
        let isPRapid = isProv strRapid
        let parseToGametype :: String -> Int -> Bool -> String -> Gametype
            parseToGametype gt valrating isprovisional i = Gametype gt valrating isprovisional i
        let bullet = parseToGametype "bullet" getBullet isPBullet getId
        let blitz = parseToGametype "blitz" getBlitz isPBlitz getId
        let rapid = parseToGametype "rapid" getRapid isPRapid getId
        let classical = parseToGametype "classical" getClassical isPClassical getId
        _ <- mapM_ (addOrGetGametypes conn) [bullet, blitz, rapid, classical]
        return ()
    