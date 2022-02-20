module Screeps.Structure.Extension where

import UPrelude
import Screeps.Data
import Screeps.FFI

store ∷ Extension → Store
store = unsafeField "store"
