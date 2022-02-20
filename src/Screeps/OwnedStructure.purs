module OwnedStructure where

import UPrelude
import Screeps.Data
import Screeps.FFI

my ∷ ∀ α. OwnedStructure α → Boolean
my = unsafeField "my"

owner ∷ ∀ α. OwnedStructure α → { username ∷ String }
owner = unsafeField "owner"
