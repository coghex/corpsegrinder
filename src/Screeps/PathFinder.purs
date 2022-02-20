module Screeps.PathFinder where

import UPrelude
import Data.Map as M
import Effect (Effect)
import Screeps.Data
import Screeps.FFI

-- TODO: options
search ∷ PathFinder → RoomPosition → Array Goal → Effect ReturnPath
search = runThisEffFn2 "search"

-- TODO: cost matrix
