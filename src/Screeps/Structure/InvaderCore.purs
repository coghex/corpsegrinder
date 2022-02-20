module Screeps.Structure.InvaderCore where

import UPrelude
import Screeps.Data
import Screeps.FFI

level ∷ InvaderCore → Int
level = unsafeField "level"

ticksToDeploy ∷ InvaderCore → Int
ticksToDeploy = unsafeField "ticksToDeploy"

spawning ∷ InvaderCore → Spawning
spawning = unsafeField "spawning"
