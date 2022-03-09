module Screeps.WorldMap where

import UPrelude
import Screeps.Data
import Screeps.FFI

describeExits ∷ WorldMap → String → Maybe ExitsInfo
describeExits = runThisFn1 "describeExits"

-- TODO: options
findExit ∷ WorldMap → String → String → ReturnCode
findExit = runThisFn2 "findExit"

findRoomExit ∷ WorldMap → Room → Room → ReturnCode
findRoomExit = runThisFn2 "findExit"

findRoute ∷ WorldMap → String → String → RoomRoute
findRoute = runThisFn2 "findRoute"

findRoomRoute ∷ WorldMap → Room → Room → RoomRoute
findRoomRoute = runThisFn2 "findRoute"

getRoomLinearDistance ∷ WorldMap → String → String → Int
getRoomLinearDistance = runThisFn2 "getRoomLinearDistance"

getRoomTerrain ∷ WorldMap → String → RoomTerrain
getRoomTerrain = runThisFn1 "getRoomTerrain"

getWorldSize ∷ WorldMap → Int
getWorldSize = runThisFn0 "getWorldSize"

getRoomStatus ∷ WorldMap → String → RoomStatus
getRoomStatus = runThisFn1 "getRoomStatus"

-- TODO: Game.map.visual
