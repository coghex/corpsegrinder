module Screeps.Room where

import UPrelude
import Effect (Effect)
import Screeps.Data
import Screeps.FFI

foreign import getRoomGlobal ∷ Effect RoomGlobal

controller ∷ Room → Controller
controller = unsafeField "controller"

energyAvailable ∷ Room → Int
energyAvailable = unsafeField "energyAvailable"

energyCapacityAvailable ∷ Room → Int
energyCapacityAvailable = unsafeField "energyCapacityAvailable"

-- TODO: implement room memory
--memory ∷ ∀ α. Room → 
--memory = unsafeField "memory"

name ∷ Room → String
name = unsafeField "name"

storage ∷ Room → Storage
storage = unsafeField "storage"

terminal ∷ Room → Terminal
terminal = unsafeField "terminal"

-- visual ∷ Room

serializePath ∷ Room → Path → String
serializePath = runThisFn1 "serializePath"

deserializePath ∷ Room → String → Path
deserializePath = runThisFn1 "deserializePath"

createConstructionSite ∷ ∀ α. Room
  → TargetPosition α → StructureType → Effect ReturnCode
createConstructionSite room (TargetPt x' y') strucType
  = runThisEffFn3 "createConstructionSite" room x' y' strucType
createConstructionSite room (TargetPos pos)  strucType
  = runThisEffFn2 "createConstructionSite" room pos   strucType
createConstructionSite room (TargetObj obj)  strucType
  = runThisEffFn2 "createConstructionSite" room obj strucType

createFlag ∷ ∀ α. Room → TargetPosition α → Effect ReturnCode
createFlag room (TargetPt x' y') = runThisEffFn2 "createFlag" room x' y'
createFlag room (TargetPos pos)  = runThisEffFn1 "createFlag" room pos
createFlag room (TargetObj obj)  = runThisEffFn1 "createFlag" room obj

find ∷ ∀ α. Room → FindType α → Array α
find = runThisFn1 "find"

find' ∷ ∀ α. Room → FindType α → FilterFn α → Array α
find' room findType filter = runThisFn2 "find" room findType { filter }

foreign import findExitToImpl ∷ ∀ α. Room → α
  → (ReturnCode → Either ReturnCode (FindType RoomPosition))
  → (FindType RoomPosition → Either ReturnCode (FindType RoomPosition)) →
  Either ReturnCode (FindType RoomPosition)

findExitTo ∷ Room → RoomIdentifier → Either ReturnCode (FindType RoomPosition)
findExitTo room (RoomName otherRoomName) = findExitToImpl room otherRoomName Left Right
findExitTo room (RoomObj  otherRoom)     = findExitToImpl room otherRoom Left Right

findPath ∷ Room → RoomPosition → RoomPosition → Path
findPath = runThisFn2 "findPath"

findPath' ∷ ∀ α. Room → RoomPosition → RoomPosition → PathOptions α → Path
findPath' room pos1 pos2 opts = runThisFn3 "findPath" room pos1 pos2 (selectMaybes opts)

getEventLog ∷ Room → Boolean → Array Event
getEventLog = runThisFn1 "getEventLog"

getPositionAt ∷ Room → Int → Int → RoomPosition
getPositionAt = runThisFn2 "getPositionAt"

getTerrain ∷ Room → RoomTerrain
getTerrain = runThisFn0 "getTerrain"

-- TODO: finish implementation of the following
-- lookAt
-- lookAtArea
-- lookForAtArea

-- lookForAt ∷ ∀ α. Room → LookType α → TargetPosition α → Array α
-- lookForAt room lookType (TargetPt x' y') = runThisFn3 "lookForAt" room lookType x' y'
-- lookForAt room lookType (TargetPos pos)  = runThisFn2 "lookForAt" room lookType pos
-- lookForAt room lookType (TargetObj obj)  = runThisFn2 "lookForAt" room lookType obj
