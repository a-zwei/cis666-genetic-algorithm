module GraphColorGA (Color, {-colorGraph,-} nodeColor) where

import Control.Monad (replicateM)
import Data.Function (on)
import Data.Ix (inRange)
import Data.List (findIndex, nub, sortBy)
import System.Random (randomIO, randomRIO)

import UGraph
import Util

type Color = Int

type Solution = [Color]

type Population = [Solution]

type Pair = (Solution, Solution)

a = 500
b = 5

fitness :: UGraph -> Solution -> Float
fitness g s = max 0 $ a / numColors s - b * adjacentSame g s

adjacentSame :: UGraph -> Solution -> Float
adjacentSame g s = sum [1 | n <- nodes g,
  nodeColor s n `elem` neighborColors g s n]

nodeColor :: Solution -> Node -> Color
nodeColor s n = s !! (n - 1) -- assuming nodes are [1..]

numColors :: Solution -> Float
numColors = fromIntegral . length . nub

cross :: Int -> Pair -> Pair
cross n (s1, s2) = (take n s1 ++ drop n s2, take n s2 ++ drop n s1)

crossIO :: Pair -> IO Pair
crossIO sp = do
  n <- randomRIO (1, (length . fst) sp - 1)
  return $ cross n sp

mutate :: Int -> Solution -> Solution
mutate ix s = update ix (color `mod` maximum s) s
  where color = s !! ix - 1

mutateIO :: Solution -> IO Solution
mutateIO s = do
  ix <- randomRIO (0, length s - 1)
  return $ mutate ix s

probs :: UGraph -> Population -> [Float]
probs g p = [fitness g s / totalFitness | s <- p]
  where totalFitness = sum $ map (fitness g) p

probRanges :: UGraph -> Population -> [(Int, Int)]
probRanges g p = zip ms (map (subtract 1) $ tail ms)
  where ms = scanl f 0 $ probs g p
        f t p = floor (10000 * p) + t

select :: UGraph -> Population -> IO Solution
select g p = do
  n <- randomRIO (0, 9999)
  let ix = fromJust $ findIndex (flip inRange n) (probRanges g p)
  return $ p !! ix

matingPool :: UGraph -> Population -> IO Population
matingPool g p = replicateM (length p) $ select g p

pairOff :: Population -> [Pair]
pairOff [] = []
pairOff (s1:s2:ss) = (s1, s2) : pairOff ss

nextGeneration :: UGraph -> Population -> IO Population
nextGeneration g p = do
  mp <- matingPool g p
  ngps <- mapM crossIO $ pairOff mp
  return $ flatten ngps

randomSolution :: Int -> Int -> IO Solution
randomSolution n max = replicateM n $ randomRIO (1, max)

randomPopulation :: Int -> Int -> Int -> IO Population
randomPopulation n sl max = replicateM n $ randomSolution sl max

neighborColors :: UGraph -> Solution -> Node -> [Color]
neighborColors g s node = [nodeColor s n | n <- neighbors g node]

best :: UGraph -> Population -> Solution
best g p = last $ sortBy (compare `on` (fitness g)) p

repeatGA :: UGraph -> Int -> Int -> Population -> IO Population
repeatGA g times regressChances p = do
  newPopulation <- iterateM times (nextGeneration g) p
  let oldBest = best g p
  let newBest = best g newPopulation
  putStr $ "Best after " ++ show times ++ " iterations: " ++ show newBest
  putStrLn $ " (fitness " ++ show (fitness g newBest) ++ ")"
  if fitness g newBest > fitness g oldBest
    then repeatGA g times regressChances newPopulation
    else if regressChances > 0
            then repeatGA g times (regressChances - 1) newPopulation
            else return p

colorGraph :: UGraph -> Int -> Int -> Int -> IO Solution
colorGraph g popSize iters regressChances = do
  initialPopulation <- randomPopulation popSize (noNodes g) (noNodes g)
  newPopulation <- repeatGA g iters regressChances initialPopulation
  return $ best g newPopulation
