module Maths where

import UPrelude
import Math (sqrt)
import Data.Maybe (Maybe(..))
import Data.Array (uncons, length, reverse, elem
                  , head, tail, drop, take)
import Data.Int(round, toNumber, quot)

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

-- shuffles a list, giving some pseudorandomness
shuffleList ∷ ∀ α. Array α → Array α
shuffleList list = mergeArrays front back
  where front = take size list
        back  = reverse $ drop size list
        size  = (length list) `quot` 2
mergeArrays ∷ ∀ α. Array α → Array α → Array α
mergeArrays []    []   = []
mergeArrays []    back = b <> mergeArrays [] back'
  where b = case uncons back of
              Just {head:b0, tail:_} → [b0]
              Nothing                → []
        back' = case uncons back of
                  Just {head:_, tail:b1} → b1
                  Nothing                → []
mergeArrays front []   = f <> mergeArrays front' []
  where f = case uncons front of
              Just {head:f0, tail:_} → [f0]
              Nothing                → []
        front' = case uncons front of
                   Just {head:_, tail:f1} → f1
                   Nothing                → []
mergeArrays front back = f <> b <> mergeArrays front' back'
  where f = case uncons front of
              Just {head:f0, tail:_} → [f0]
              Nothing                → []
        front' = case uncons front of
                   Just {head:_, tail:f1} → f1
                   Nothing                → []
        b = case uncons back of
              Just {head:b0, tail:_} → [b0]
              Nothing                → []
        back' = case uncons back of
                  Just {head:_, tail:b1} → b1
                  Nothing                → []


-- | removes one array from another
subtractSet ∷ ∀ α. (Eq α) ⇒ Array α → Array α → Array α
subtractSet [] [] = []
subtractSet [] b  = []
subtractSet a  [] = a
subtractSet a  b  = val <> subtractSet a' b
  where val = case uncons a of
                Nothing              → []
                Just {head:h,tail:_} → if (h `elem` b) then [] else [h]
        a'  = case uncons a of
                Nothing              → []
                Just {head:_,tail:t} → t
