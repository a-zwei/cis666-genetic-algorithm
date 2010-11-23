module GraphColorGA (Color, {-colorGraph,-} nodeColor) where

import Data.Array.Unboxed
import Data.List (delete, nub, sortBy)
import System.Random

import UGraph
import Util

type Color = Int

type Solution = [Color]

type Population = [Solution]

a = 100
b = 10

fitness :: UGraph -> Solution -> Float
fitness g s = a / numColors s - b * adjacentSame g s

adjacentSame :: UGraph -> Solution -> Float
adjacentSame g s = sum [1 | n <- nodes g, nodeColor s n `elem` neighborColors g s n] / 2

numColors :: Solution -> Float
numColors = fromIntegral . length . nub

cross :: Int -> Solution -> Solution -> (Solution, Solution)
cross n s1 s2 = (take n s1 ++ drop n s2, take n s2 ++ drop n s1)

--mutate :: Int -> Solution -> Solution
--mutate seed s =
--  where ix = randomR (1, length s) (mkStdGen seed) - 1

--update :: UGraph -> Population -> Population
--update g s = State $ array (bounds $ us s) (map updateU $ assocs (us s))
--  where updateU ((i, j), u) = ((i, j), u + dt * dudt g s (i, j))

randomSolution :: Int -> Int -> Int -> Solution
randomSolution n max seed = take n $ randomRs (1, max) (mkStdGen seed)

nodeColor :: Solution -> Node -> Color
nodeColor s n = s !! n - 1 -- assuming nodes are [1..]

neighborColors :: UGraph -> Solution -> Node -> [Color]
neighborColors g s node = [nodeColor s n | n <- neighbors g node]

--colorGraph :: UGraph -> Int -> Solution
--colorGraph g seed = states !! localMin energies
--  where initialState = randomState (noNodes g) (noNodes g) seed
--        states = filter valid $ iterate (update g) initialState
--        energies = map (energy g) states
--        valid s = allColored s && neighborsDiffer g s