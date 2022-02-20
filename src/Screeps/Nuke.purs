module Screeps.Nuke where

import UPrelude
import Screeps.Data
import Screeps.FFI

id ∷ Nuke → String
id = unsafeField "id"

launchRoomName ∷ Nuke → String
launchRoomName = unsafeField "launchRoomName"

timeToLand ∷ Nuke → Int
timeToLand = unsafeField "timeToLand"
