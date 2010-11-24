module Util where

boolToInt True = 1
boolToInt False = 0

fromJust (Just a) = a

update ix v vs = take ix vs ++ v : drop (ix + 1) vs

clamp (l, u) v = min u $ max l v
