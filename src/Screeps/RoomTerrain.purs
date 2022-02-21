module Screeps.RoomTerrain where

import UPrelude
import Effect (Effect)
import Data.Maybe (Maybe)
import Screeps.Data
import Screeps.FFI

-- TODO: finish this, i dont think its right, need raw function
constructor ∷ String → Effect RoomTerrain
constructor = runThisEffFn0 "constructor"

get ∷ RoomTerrain → Int → Int → Maybe TerrainMask
get rt x y = toMaybe $ runThisFn2 "get" rt x y

-- TODO: getRawBuffer
