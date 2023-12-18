{-# LANGUAGE DeriveGeneric #-}
-- | This module defines the data types used in this application
module Types (
    -- * Type Aliases
    Weight,
    Bias,
    LR,
    Input,
    Output,
    -- * Custom Exceptions
    CustomException(..),
    -- * Data Types
    Gamedata (..),
    Player (..),
    Playerinfo (..),
    Gametype (..)
) where

import Control.Exception
import GHC.Generics
import Data.Aeson (ToJSON)

-- | Type Synonyms
-- Represents the weight used in the classifier.
type Weight = Double
-- | Represents the bias used in the classifier.
type Bias = Double
-- | Represents the learning rate in the classifier.
type LR = Double
-- | Represents the input feature for the classifier.
type Input = Double
-- | Represents the output or target variable for the classifier.
type Output = Double

-- | Datatypes
-- Custom exceptions for various error conditions that can arise in the application.
data CustomException = InitModelException | NoPlayerFException | PredictionError | DatabaseEmptyError

instance Show CustomException where
    show InitModelException = "ERROR: Model Initialisation Failed"
    show NoPlayerFException = "ERROR: Player Not Found"
    show PredictionError    = "ERROR: Model Failed to Predict Winner"
    show DatabaseEmptyError = "ERROR: Database Contains No Data"
    
instance Exception CustomException

-- | Represents game data including the game ID, the rating differential, and the outcome.
data Gamedata = Gamedata{
        iD :: Int,
        differential :: Double,
        outcome :: Double
    } deriving (Eq, Show, Ord, Generic)
instance ToJSON Gamedata

-- | Represents player data including username, profile URL, and game statistics.
data Player = Player{
        username :: String,
        url :: String,
        games :: Int,
        wins :: Int,
        draws :: Int,
        losses :: Int
    } deriving (Eq, Show, Ord, Generic)
instance ToJSON Player

-- | Represents an output join of the Player and Gametype datatypes when retrieving from the database.
data Playerinfo = Playerinfo{
        username_ :: String,
        url_ :: String,
        games_ :: Int,
        wins_ :: Int,
        draws_ :: Int,
        losses_ :: Int,
        info_ :: String
    } deriving (Eq, Show, Ord)

-- | Represents an entry for a user rating including the game type, the rating, if the rating is provisional, and the user.
data Gametype = Gametype{
        gtype :: String,
        rating :: Int,
        prov :: Bool,
        user :: String
    } deriving (Eq, Show, Ord, Generic)
instance ToJSON Gametype