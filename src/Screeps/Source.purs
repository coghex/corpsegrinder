module Screeps.Source where

import UPrelude
import Screeps.Data
import Screeps.FFI

energy ∷ Source → Int
energy = unsafeField "energy"

energyCapacity ∷ Source → Int
energyCapacity = unsafeField "energyCapacity"

id ∷ Source → Id Source
id = unsafeField "id"

ticksToRegeneration ∷ Source → Int
ticksToRegeneration = unsafeField "ticksToRegeneration"
