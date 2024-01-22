-- | A simple concurrent messaging simulation between users.

module Main (main) where

import Control.Concurrent
import Control.Monad
import System.Random
import Data.Map.Strict as Map
import Data.List (intercalate)

-- | A message with a sender and the text of the message
data Message = Message {
    from :: String,
    text :: String
} deriving Show

-- | A simulated user holding their name, list of received messages, and their list of friends
data User = User { 
    name :: String, 
    messages :: MVar [Message]
}

instance Eq User where
    u1 == u2 = name u1 == name u2

instance Show User where
    show = name

-- | Main function that initializes the simulation and prints results.
main :: IO ()
main = do
    -- Create the users
    users <- mapM (createUser . show) [1..10]
    let len = length users
    -- Count up to 100 messages
    numMessages <- newMVar 0
    -- Map where the keys are pairs of users, and the value states their friendship as a Boolean
    friendship <- newMVar $ Map.fromList [((name $ users !! i, name $ users !! j), True) | i <- [0..len - 1], j <- [0..len - 1]]
    check <- newEmptyMVar
    -- Create user threads
    threads <- mapM (forkIO . userThread users check numMessages) users
    -- 100 messages have been sent
    complete <- takeMVar check

    -- Updating friendship status based on message exchanges.
    mapM_ (\user -> do
        friendsMap <- takeMVar friendship
        messages <- readMVar $ messages user
        -- Function for checking if other users have sent the current user a message
        let senders x = x `elem` Prelude.map from messages
        -- Update the friendship Map for each user
        updatedFriendsMap <- updateMemo user (Prelude.map (senders . name) users) users friendsMap
        putMVar friendship updatedFriendsMap) users

    -- Printing user details.
    friendsMap <- readMVar friendship
    mapM_ (\user -> do
        messages <- readMVar $ messages user
        let messageCount = length messages
        -- Checks if two users are friends based on the friendship map, and returns their friendship status as Bool
        let friends = Prelude.map name $ Prelude.filter (\other -> do
                case Map.lookup (name user, name other) friendsMap of
                    Just status -> status
                    Nothing -> False) users
        putStrLn $ "User: " ++ name user
        putStrLn $ "Messages received: " ++ show messageCount
        putStrLn $ "Friends: " ++ intercalate ", " friends
        putStrLn "") users   

-- | Updates the friendship status map for a given user.
updateMemo :: User -> [Bool] -> [User] -> Map.Map (String, String) Bool -> IO (Map.Map (String, String) Bool)
updateMemo currentUser statuses users memo = do
    -- Create pairs of other users and their friendship status to the current user
    let statuspairs = zip users statuses
        updatedMemo = Prelude.foldl (updateStatus currentUser) memo statuspairs
    return updatedMemo
        where
            -- Updates the status between two users in the map, using memoization to reduce number of calculations
            updateStatus user originalMap (otherUser, status) =
                -- Performs logical AND comparison to update each entry in the frienship map
                let updatedMap1 = Map.adjust (&& status) (name user, name otherUser) originalMap
                    updatedMap2 = Map.adjust (&& status) (name otherUser, name user) updatedMap1
                in updatedMap2

-- | Creates a new user with a given name.
createUser :: String -> IO User
createUser name = do
    messages <- newMVar []
    return User { name = name, messages = messages }

-- | Represents the behavior of a user in a separate thread.
userThread :: [User] -> MVar Bool -> MVar Int -> User -> IO ()
userThread users check numMessages currentUser = do
    -- takes MVar randomly between 1 and 5 seconds
    delay <- randomRIO (1, 5)
    threadDelay (delay * 1000000)
    curMessage <- takeMVar numMessages
    -- End program if 100 messages total across threads have been sent
    if curMessage == 100 then
        putMVar check True
    -- Otherwise send message to another random user and update the total messages count
    else do
        let others = Prelude.filter (/= currentUser) users
        index <- randomRIO (0, length others - 1)
        let otherUser = others !! index
        sendMessage currentUser otherUser $ show (curMessage + 1)
        putMVar numMessages (curMessage + 1)
        userThread users check numMessages currentUser

-- | Sends a message from one user to another.
sendMessage :: User -> User -> String -> IO ()
sendMessage user other text = do
    otherMessages <- takeMVar $ messages other
    putMVar (messages other) (Message {from = name user, text = text} : otherMessages)
