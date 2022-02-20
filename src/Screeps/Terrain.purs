module Terrain where

import UPrelude
import Effect (Effect)
import Screeps.Data
import Screeps.FFI

constructor ∷ String → Effect RoomTerrain
constructor = runThisEffFn0 "constructor"

-- TODO: finish this
