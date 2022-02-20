module Screeps.Structure.KeeperLair where

import UPrelude
import Screeps.Data
import Screeps.FFI

ticksToSpawn ∷ KeeperLair → Int
ticksToSpawn = unsafeField "ticksToSpawn"
