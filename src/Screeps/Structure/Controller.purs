module Screeps.Structure.Controller where

import UPrelude
import Effect (Effect)
import Screeps.Data
import Screeps.FFI

isPowerEnabled ∷ Controller → Boolean
isPowerEnabled = unsafeField "isPowerEnabled"

level ∷ Controller → Int
level = unsafeField "level"

progress ∷ Controller → Int
progress = unsafeField "progress"

progressTotal ∷ Controller → Int
progressTotal = unsafeField "progressTotal"

reservation ∷ Controller → Reservation
reservation = unsafeField "reservation"

safeMode ∷ Controller → Int
safeMode = unsafeField "safeMode"

safeModeAvailable ∷ Controller → Int
safeModeAvailable = unsafeField "safeModeAvailable"

safeModeCooldown ∷ Controller → Int
safeModeCooldown = unsafeField "safeModeCooldown"

sign ∷ Controller → Sign
sign = unsafeField "sign"

ticksToDowngrade ∷ Controller → Int
ticksToDowngrade = unsafeField "ticksToDowngrade"

upgradeBlocked ∷ Controller → Int
upgradeBlocked = unsafeField "upgradeBlocked"

activateSafeMode ∷ Controller → Effect ReturnCode
activateSafeMode = runThisEffFn0 "activateSafeMode"

unclaim ∷ Controller → Effect ReturnCode
unclaim = runThisEffFn0 "unclaim"
