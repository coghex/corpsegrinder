module Maths where

import UPrelude
import Math (sqrt)
import Data.Maybe (Maybe(..))
import Data.Array (uncons)
import Data.Int(round, toNumber)

-- | returns rounded distance between two points
distance ∷ Int → Int → Int → Int → Int
distance x0 y0 x1 y1 = round $ sqrt $ (x1' - x0') + (y1' - y0')
  where x0' = toNumber x0
        y0' = toNumber y0
        x1' = toNumber x1
        y1' = toNumber y1

-- | returns index of the smallest item in array
findMin ∷ Array Int → Int
findMin arr = findMinF arr 0 0 1000000
findMinF ∷ Array Int → Int → Int → Int → Int
findMinF []  _ minn _   = minn
findMinF arr n minn val = if (val' < val) then
  findMinF arr' (n + 1) n val'
  else findMinF arr' (n + 1) minn val
  where val' = case uncons arr of
                 Nothing                → 1000000
                 Just {head:v0, tail:_} → v0
        arr' = case uncons arr of
                 Nothing                → []
                 Just {head:_, tail:a0} → a0
