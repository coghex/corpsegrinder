module Screeps.Structure.Lab where

import UPrelude
import Effect (Effect)
import Screeps.Data
import Screeps.FFI

cooldown ∷ Lab → Int
cooldown = unsafeField "cooldown"

mineralType ∷ Lab → ResourceType
mineralType = unsafeField "mineralType"

store ∷ Lab → Store
store = unsafeField "store"

boostCreep ∷ Lab → Creep → Effect ReturnCode
boostCreep = runThisEffFn1 "boostCreep"

boostCreep' ∷ Lab → Creep → Int → Effect ReturnCode
boostCreep' = runThisEffFn2 "boostCreep"

reverseReaction ∷ Lab → Lab → Lab → Effect ReturnCode
reverseReaction = runThisEffFn2 "reverseReaction"

runReaction ∷ Lab → Lab → Lab → Effect ReturnCode
runReaction = runThisEffFn2 "runReaction"

unboostCreep ∷ Lab → Creep → Effect ReturnCode
unboostCreep = runThisEffFn1 "unboostCreep"
