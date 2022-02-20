module Screeps.Mineral where

import UPrelude
import Screeps.Data
import Screeps.FFI

density ∷ Mineral → Density
density = unsafeField "density"

mineralAmount ∷ Mineral → Int
mineralAmount = unsafeField "mineralAmount"

mineralType ∷ Mineral → ResourceType
mineralType = unsafeField "mineralType"

id ∷ Mineral → Id Mineral
id = unsafeField "id"

ticksToRegeneration ∷ Mineral → Int
ticksToRegeneration = unsafeField "ticksToRegeneration"
