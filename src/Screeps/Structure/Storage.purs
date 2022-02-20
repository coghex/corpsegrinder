module Screeps.Structure.Storage where

import UPrelude
import Screeps.Data
import Screeps.FFI

store ∷ Storage → Store
store = unsafeField "store"
