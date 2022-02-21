module Screeps.RoomTerrain where

import UPrelude
import Effect (Effect)
import Data.Maybe (Maybe)
import Screeps.Data
import Screeps.FFI

-- TODO: finish this, i dont think its right, need raw function
constructor ∷ String → Effect RoomTerrain
constructor = runThisEffFn0 "constructor"

-- figure out why this doesnt return terrain mask
--get ∷ RoomTerrain → Int → Int → Maybe TerrainMask
get ∷ RoomTerrain → Int → Int → Maybe Int
get rt x y = toMaybe $ runThisFn2 "get" rt x y

-- TODO: getRawBuffer
