-- | This module provides functionality for training a logistic regression model,
-- including data normalization, training step execution, and accuracy testing.

module Model (
    modelInitialise,
    comparePlayers
) where

import System.Random
import Data.String (fromString)
import Database.SQLite.Simple 
import Types
import Database

-- | Combines two lists of 'Input' and 'Output' into a list of tuples.
combine :: [Input] -> [Output] -> [(Input, Output)]
combine [] [] = []
combine (x:xs) (y:ys) = (x, y) : combine xs ys

-- | Calculates the mean of the differentials in a list of 'Gamedata'.
mean :: [Gamedata] -> Double
mean games =
    let sumDiffs = sum $ map differential games
        count = fromIntegral $ length games
    in sumDiffs / count

-- | Calculates the standard deviation of the differentials in a list of 'Gamedata'.
sdev :: [Gamedata] -> Double
sdev games =
    let average = mean games
        sumSqrs = sum $ map (\x -> (differential x - average)^2) games
        count = fromIntegral $ length games
    in sqrt $ sumSqrs / count

-- | Normalizes the differentials in a list of 'Gamedata' using the provided mean and standard deviation.
norm :: [Gamedata] -> Double -> Double -> [Input]
norm games mu sd = map (\g -> (differential g - mu) / sd) games

-- | Sigmoid activation function used in logistic regression.
sigmoid :: Double -> Double
sigmoid z = 1 / (1 + exp (-z))

-- | Logistic regression function that calculates the probability of an outcome.
logisticRegression :: Weight -> Bias -> Input -> Output
logisticRegression w b x = sigmoid $ w * x + b

-- | Performs a single training step and updates the weight and bias.
trainStep :: [(Input, Output)] -> LR -> Weight -> Bias -> (Weight, Bias)
trainStep dataPoints lr w b = train dataPoints (0, 0) n
    where
        n = fromIntegral $ length dataPoints
        train :: [(Input, Double)] -> (Weight, Bias) -> Double -> (Weight, Bias)
        train dataPoints (wGrad, bGrad) n
            | dataPoints == [] = (w - lr * (wGrad / n), b - lr * (bGrad / n))
            | otherwise = 
                let (x, y) = head dataPoints
                    prediction = logisticRegression w b x
                    err = prediction - y
                in train (tail dataPoints) (wGrad + err * x, bGrad + err) n

-- | Trains the logistic regression model for a given number of epochs.
trainModel :: [(Input, Output)] -> Int -> LR -> Weight -> Bias -> (Weight, Bias)
trainModel dataPoints epochs lr w b 
    | epochs == 0 = (w, b)
    | otherwise = do
        let (nW, nB) = trainStep dataPoints lr w b
        trainModel dataPoints (epochs - 1) lr nW nB

-- | Classifies the output of the logistic regression as 0 or 1 based on a threshold.
classify :: Output -> Double
classify x = if (x > 0.5) then 1.0 else 0.0

-- | Checks if the prediction made by the logistic regression model is correct.
isCorrect :: Weight -> Bias -> (Input, Output) -> Double
isCorrect w b (x, truth) = if classify (logisticRegression w b x) == truth then 1.0 else 0.0

-- | Calculates the accuracy of the logistic regression model on a dataset.
testAccuracy :: [(Input, Output)] -> Weight -> Bias -> Double
testAccuracy dataPoints w b =
    let count = fromIntegral $ length dataPoints
        correct = sum $ map (isCorrect w b) dataPoints
    in 100 * (correct / count)

-- | Compares two players by predicting the outcome of a match between them using the model.
comparePlayers :: Double -> Double -> Weight -> Bias -> Double
comparePlayers av1 av2 w b = classify $ logisticRegression w b (av1 - av2)

-- | Initializes the logistic regression model by training on data from the database.
-- Returns the final weight and bias of the trained model.
modelInitialise :: Connection -> IO (Weight, Bias)
modelInitialise conn = do
    let select = fromString "SELECT * FROM games"
    results <- queryNamed conn select [] :: IO [Gamedata]
    let lengthResults = (fromIntegral $ length results) :: Double
    let trainingData = take (floor $ 0.8 * lengthResults) results
    let testingData = drop (ceiling $ 0.8 * lengthResults) results
    let yTrainData = map outcome trainingData
    let yTestData = map outcome testingData
    let trainMean = mean trainingData
    let trainStDv = sdev trainingData
    let normXtrainData = norm trainingData trainMean trainStDv
    let normXtestData = norm testingData trainMean trainStDv
    g <- getStdGen
    let weight = head $ (randomRs (-1, 1) g :: [Weight])
    let learningRate = 0.1
    let bias = 1.0
    let epochs = 200
    let (finalW, finalB) = trainModel (combine normXtrainData yTrainData) epochs learningRate weight bias
    putStrLn "Model Training on Example Data Completed!"
    putStrLn $ "Accuracy on Testing Data: " ++ (show $ testAccuracy (combine normXtestData yTestData) finalW finalB) ++ "%"
    putStrLn $ "Model ... Final Weight: " ++ show finalW ++ ", Final Bias: " ++ show finalB
    return (finalW, finalB)