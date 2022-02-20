module Screeps.Structure.Spawning where

import UPrelude
import Effect (Effect)
import Screeps.Data
import Screeps.FFI

directions ∷ Spawning → Directions
directions = unsafeField "directions"

name ∷ Spawning → String
name = unsafeField "name"

needTime ∷ Spawning → Int
needTime = unsafeField "needTime"

remainingTime ∷ Spawning → Int
remainingTime = unsafeField "remainingTime"

spawn ∷ Spawning → Spawn
spawn = unsafeField "spawn"

cancel ∷ Spawning → Effect ReturnCode
cancel = runThisEffFn0 "cancel"

setDirections ∷ Spawning → Directions → Effect ReturnCode
setDirections = runThisEffFn1 "setDirections"
