module Screeps.Deposit where

import UPrelude
import Screeps.Data
import Screeps.FFI

cooldown ∷ Deposit → Int
cooldown = unsafeField "cooldown"

depositType ∷ Deposit → ResourceType
depositType = unsafeField "depositType"

id ∷ Deposit → Id Deposit
id = unsafeField "id"

lastCooldown ∷ Deposit → Int
lastCooldown = unsafeField "lastCooldown"

ticksToDecay ∷ Deposit → Int
ticksToDecay = unsafeField "ticksToDecay"
