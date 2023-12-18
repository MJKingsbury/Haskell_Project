-- | This module is responsible for interacting with the SQLite database. It includes
-- functions for initializing the database, exporting data to JSON, and various CRUD operations.

module Database ( 
    initialise,
    exportToJson,
    checkModelInit,
    addOrGetAllGames,
    addOrGetPlayer,
    getPlayer,
    getPlayerInfo,
    deletePlayer,
    getAverageRating,
    addOrGetGametypes
) where 

import Types 
import Control.Applicative
import Data.String (fromString)
import Data.Aeson (encode)
import Data.Char
import Data.Either
import Data.Maybe
import Database.SQLite.Simple 
import Database.SQLite.Simple.Internal
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL

-- | Example Conversion of Data from Database Row to Haskell Datatype
instance FromRow Gamedata where
    fromRow = Gamedata <$> field <*> field <*> field

-- | Example Conversion of Haskell Datatype to Row in Database
instance ToRow Gamedata where
    toRow (Gamedata iD_ differential_ outcome_) = toRow (iD_, differential_, outcome_)

instance FromRow Player where
    fromRow = Player <$> field <*> field <*> field <*> field <*> field <*> field

instance ToRow Player where
    toRow (Player username_ url_ games_ wins_ draws_ losses_) = toRow (username_, url_, games_, wins_, draws_, losses_)

instance FromRow Gametype where
    fromRow = Gametype <$> field <*> field <*> field <*> field

instance ToRow Gametype where
    toRow (Gametype gtype_ rating_ prov_ user_) = toRow (gtype_, rating_, prov_, user_)

instance FromRow Playerinfo where
    fromRow = Playerinfo <$> field <*> field <*> field <*> field <*> field <*> field <*> field

-- | Initializes the SQLite database connection and creates necessary tables if they do not exist.
initialise :: IO Connection
initialise = do 
    conn <- open "lichess.db"
    execute_ conn (fromString "CREATE TABLE IF NOT EXISTS games (\
        \iD INTEGER PRIMARY KEY, \
        \differential DOUBLE NOT NULL, \
        \outcome DOUBLE NOT NULL \
        \)")
    execute_ conn (fromString "CREATE TABLE IF NOT EXISTS players (\
        \username TEXT PRIMARY KEY, \
        \url TEXT NOT NULL, \
        \games INTEGER NOT NULL, \
        \wins INTEGER NOT NULL, \
        \draws INTEGER NOT NULL, \
        \losses INTEGER NOT NULL \
        \)")
    execute_ conn (fromString "CREATE TABLE IF NOT EXISTS gametypes (\
        \gtype TEXT NOT NULL, \
        \rating INTEGER NOT NULL, \
        \prov BOOLEAN NOT NULL, \
        \user TEXT NOT NULL, \
        \FOREIGN KEY (user) REFERENCES players (username)\
        \)")
    return conn

-- | Exports data from the SQLite database to JSON files.
exportToJson :: Connection -> IO (Either CustomException String)
exportToJson conn = do
    let selPlayers = fromString "SELECT * FROM players"
    let selGametypes = fromString "SELECT * FROM gametypes"
    let selGames = fromString "SELECT * FROM games"
    resPlayers <- queryNamed conn selPlayers [] :: IO [Player]
    resGametypes <- queryNamed conn selGametypes [] :: IO [Gametype]
    resGames <- queryNamed conn selGames [] :: IO [Gamedata]
    if (length resPlayers) + (length resGametypes) + (length resGames) == 0 
        then return $ Left DatabaseEmptyError
        else do
            let playersJson  = encode resPlayers
            let gametypesJson = encode resGametypes
            let gamesJson = encode resGames
            BL.writeFile ("json/players.json") playersJson
            BL.writeFile ("json/gametypes.json") gametypesJson
            BL.writeFile ("json/games.json") gamesJson
            return $ Right "Database successfully exported to /json."

-- | Checks if any 'Gamedata' has been initialized in the database.
-- Returns 'Nothing' if the database is empty or 'Just Gamedata' with the first row.
checkModelInit :: Connection -> IO (Maybe Gamedata)
checkModelInit conn = do
    let select = fromString "SELECT * FROM games"
    results <- queryNamed conn select [] :: IO [Gamedata]
    case results of
        [] -> return Nothing
        _  -> return $ Just (head results)

-- | Adds a list of 'Gamedata' to the database or retrieves them if they already exist.
-- Returns the list of 'Gamedata' added or retrieved.
addOrGetAllGames :: Connection -> [Gamedata] -> IO [Gamedata]
addOrGetAllGames _ [] = do
    return []
addOrGetAllGames conn li = do
    game <- addOrGetGameData conn (head li)
    rest <- addOrGetAllGames conn (tail li)
    return (game:rest)

-- | Adds 'Gamedata' to the database or retrieves it if it already exists.
-- Returns the 'Gamedata' added or retrieved.
addOrGetGameData :: Connection -> Gamedata -> IO Gamedata
addOrGetGameData conn game = do
    let select = fromString "SELECT * FROM games WHERE differential=:differential AND outcome=:outcome"
    results <- queryNamed conn select [T.pack ":differential" := differential game, T.pack ":outcome" := outcome game] :: IO [Gamedata]
    if length results == 0 then do
        let insert = fromString "INSERT INTO games (iD, differential, outcome) VALUES (?, ?, ?)"
        execute conn insert (iD game, differential game, outcome game)
        return game
    else do
        return $ head $ results

-- | Retrieves a 'Player' from the database based on their username.
-- Returns 'Nothing' if the player is not found, or 'Just Player' with the player's data.
getPlayer :: Connection -> String -> IO (Maybe Player)
getPlayer conn p = do
    let select = fromString "SELECT * FROM players WHERE username=:username"
    results <- queryNamed conn select [T.pack ":username" := p] :: IO [Player]
    case results of
        [] -> return Nothing
        _  -> return $ Just (head results)

-- | Retrieves detailed 'Playerinfo' for a given username, including associated game types and ratings.
-- Returns 'Nothing' if the player is not found, or 'Just Playerinfo' with detailed player information.
getPlayerInfo :: Connection -> String -> IO (Maybe Playerinfo)
getPlayerInfo conn p = do
    let select = fromString "SELECT players.*, \
                                \GROUP_CONCAT(gametypes.gtype || ': ' || gametypes.rating, ', ') AS info \
                                \FROM players \
                                \JOIN gametypes ON players.username = gametypes.user WHERE players.username=:username \
                                \GROUP BY players.username" 
    results <- queryNamed conn select [T.pack ":username" := p] :: IO [Playerinfo]
    case results of 
        [] -> return Nothing
        _  -> return $ Just (head results)

-- | Adds a 'Player' to the database if they do not exist, or retrieves them if they do.
-- Returns the 'Player' from the database.
addOrGetPlayer :: Connection -> Player -> IO Player
addOrGetPlayer conn p = do
    results <- getPlayer conn (username p)
    case results of
        Nothing -> do
            putStrLn "Adding player to database..."
            let insert = fromString "INSERT INTO players (username, url, games, wins, draws, losses) VALUES (?, ?, ?, ?, ?, ?)"
            execute conn insert (username p, url p, games p, wins p, draws p, losses p)
            addOrGetPlayer conn p
        Just player -> do 
            putStrLn "Player added to database."
            return player

-- | Deletes a 'Player' from the database based on their username.
-- Returns 'Nothing' if the player did not exist, or 'Just Player' with the deleted player's data.
deletePlayer :: Connection -> String -> IO (Maybe Player)
deletePlayer conn p = do
    results <- getPlayer conn p
    case results of
        Nothing -> return Nothing
        _  -> do
            let delete = fromString "DELETE FROM players WHERE username=:username"
            deletions <- executeNamed conn delete [T.pack ":username" := p] :: IO ()
            let deletegames = fromString "DELETE FROM gametypes WHERE user:=user"
            _ <- executeNamed conn deletegames [T.pack ":user" := p] :: IO ()
            return results

-- | Calculates the average rating of a 'Player' based on their non-provisional game types.
-- Returns the average rating as a 'Double'.
getAverageRating :: Connection -> Player -> IO Double
getAverageRating conn p = do
    let select = fromString "SELECT * FROM gametypes WHERE user=:user AND prov=FALSE"
    results <- queryNamed conn select [T.pack ":user" := username p] :: IO [Gametype]
    return $ (sum (map (\x -> fromIntegral $ rating x) results)) / (fromIntegral (length results))

-- | Adds a 'Gametype' to the database for a given user if it does not exist,
-- or retrieves it if it does.
-- Returns a list of 'Gametype' that has been added or found.
addOrGetGametypes :: Connection -> Gametype -> IO [Gametype]
addOrGetGametypes conn g = do
    let select = fromString "SELECT * FROM gametypes WHERE user=:user AND gtype=:gtype"
    results <- queryNamed conn select [T.pack ":user" := user g, T.pack ":gtype" := gtype g] :: IO [Gametype]
    if length results == 0 then do
        let insert = fromString "INSERT INTO gametypes (gtype, rating, prov, user) VALUES (?, ?, ?, ?)"
        execute conn insert (gtype g, rating g, prov g, user g)
        addOrGetGametypes conn g
    else do
        return results