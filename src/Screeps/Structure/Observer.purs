module Screeps.Structure.Observer where

import UPrelude
import Effect (Effect)
import Screeps.Data
import Screeps.FFI

observeRoom ∷ Observer → String → Effect ReturnCode
observeRoom = runThisEffFn1 "observeRoom"
