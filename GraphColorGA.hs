module GraphColorGA (Color, colorGraph, nodeColor, Solution) where

import Control.Monad (liftM, when, replicateM)
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
  where ms = fixlast $ scanl f 0 $ probs g p
        f t p = round (10000 * p) + t
        fixlast xs = reverse . (10000:) $ (tail . reverse) xs

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

mutation = 10000 :: Int

nextGeneration :: UGraph -> Population -> IO Population
nextGeneration g p = do
  mp <- matingPool g p
  ngps <- liftM flatten $ mapM crossIO $ pairOff mp
  r <- randomRIO (1, mutation)
  rix <- randomRIO (0, length p - 1)
  ms <- mutateIO (ngps !! rix)
  return $ if r == 1 then update rix ms ngps else ngps

randomSolution :: Int -> Int -> IO Solution
randomSolution len max = replicateM len $ randomRIO (1, max)

randomPopulation :: Int -> Int -> Int -> IO Population
randomPopulation n slen max = replicateM n $ randomSolution slen max

neighborColors :: UGraph -> Solution -> Node -> [Color]
neighborColors g s node = [nodeColor s n | n <- neighbors g node]

best :: UGraph -> Population -> Solution
best g p = last $ sortBy (compare `on` (fitness g)) p

repeatGA :: UGraph -> Int -> Int -> Int -> Solution -> Population
  -> IO Population
repeatGA g betweenLog iters regressChances bestBest p = do
  newPop <- nextGeneration g p
  let oldBest = best g p
      newBest = best g newPop
      oldBestF = fitness g oldBest
      newBestF = fitness g newBest
      bestBestF = fitness g bestBest
  when (iters == 0) $
    putStrLn $ "Best " ++ show newBest ++ " (fitness " ++ show newBestF ++ ")"
  let ni = (iters + 1) `mod` betweenLog
  if newBestF > oldBestF
    then if newBestF > bestBestF
            then repeatGA g betweenLog ni regressChances newBest newPop
            else repeatGA g betweenLog ni (regressChances + 1) bestBest newPop
    else if regressChances > 0
            then repeatGA g betweenLog ni (regressChances - 1) bestBest newPop
            else return [bestBest]

colorGraph :: UGraph -> Int -> Int -> Int -> IO Solution
colorGraph g popSize iters regressChances = do
  initialPopulation <- randomPopulation popSize (noNodes g) (noNodes g)
  let initialBest = best g initialPopulation
  newPop <- repeatGA g iters 0 regressChances initialBest initialPopulation
  return $ best g newPop
