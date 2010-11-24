module Util where

import Control.Monad (foldM)

boolToInt True = 1
boolToInt False = 0

fromJust (Just a) = a

update ix v vs = take ix vs ++ v : drop (ix + 1) vs

clamp (l, u) v = min u $ max l v

flatten [] = []
flatten ((a, b):xs) = a:b:flatten xs

iterateM :: Monad m => Int -> (a -> m a) -> a -> m a
iterateM n mf init = foldM (\p m -> m p) init (replicate n mf)
