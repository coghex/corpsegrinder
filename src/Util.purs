module Util where

import UPrelude
import Math (sqrt)
import Data.Int(round, toNumber)

-- | returns rounded distance between two points
distance ∷ Int → Int → Int → Int → Int
distance x0 y0 x1 y1 = round $ sqrt $ (x1' - x0') + (y1' - y0')
  where x0' = toNumber x0
        y0' = toNumber y0
        x1' = toNumber x1
        y1' = toNumber y1
