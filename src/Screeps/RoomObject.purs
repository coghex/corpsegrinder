module Screeps.RoomObject where

import UPrelude
import Screeps.Data
import Screeps.FFI

pos ∷ ∀ α. RoomObject α → RoomPosition
pos = unsafeField "pos"

room ∷ ∀ α. RoomObject α → Room
room = unsafeField "room"

storeMaybe ∷ ∀ α. RoomObject α → Maybe Store
storeMaybe struct = toMaybe $ unsafeField "store" struct
