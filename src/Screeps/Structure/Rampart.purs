module Screeps.Structure.Rampart where

import UPrelude
import Effect (Effect)
import Screeps.Data
import Screeps.FFI

isPublic ∷ Rampart → Boolean
isPublic = unsafeField "isPublic"

ticksToDecay ∷ Rampart → Int
ticksToDecay = unsafeField "ticksToDecay"

setPublic ∷ Rampart → Boolean → Effect ReturnCode
setPublic = runThisEffFn1 "setPublic"
