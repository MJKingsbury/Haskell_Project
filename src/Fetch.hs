{-# LANGUAGE OverloadedStrings #-}
-- | Module responsible for fetching data from the Lichess API. It provides functions
-- to fetch games and player data using HTTP requests.
module Fetch (
    -- * Constants
    numgamesfetch,
    -- * Fetch Functions
    fetchGames,
    fetchPlayer
) where

import Network.HTTP.Simple
import qualified Data.ByteString.Lazy.Char8 as LC8

-- | The number of games to fetch in one request.
numgamesfetch = 200 :: Int

-- | Fetches a list of games for a given user from the Lichess API.
--
-- @param username The Lichess username for which to fetch games.
-- @return A 'String' containing the raw JSON response with game data.
fetchGames :: String -> IO String
fetchGames username = do
    request <- parseRequest $ "https://lichess.org/api/games/user/" ++ username ++ "?max=" ++ (show numgamesfetch) ++ "&moves=false"
    response <- httpLBS request
    return $ LC8.unpack $ getResponseBody response

-- | Fetches player information for a given user from the Lichess API.
--
-- @param username The Lichess username for which to fetch player information.
-- @return A 'String' containing the raw JSON response with player information.
fetchPlayer :: String -> IO String
fetchPlayer username = do
    request <- parseRequest $ "https://lichess.org/api/user/" ++ username
    response <- httpLBS request
    return $ LC8.unpack $ getResponseBody response


