module Screeps.RoomPosition where

import UPrelude
import Effect (Effect)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Screeps.Data
import Screeps.FFI


foreign import createRoomPosition ∷ Int → Int → String → RoomPosition
--constructor ∷ Int → Int → String → Effect RoomPosition
--constructor = runThisEffFn3 "constructor"

-- TODO: find a replacement for runPure
-- tryPure ∷ ∀ α. Effect α → Either Error α
-- tryPure = runPure <<< try

roomName ∷ RoomPosition → String
roomName = unsafeField "roomName"

x ∷ RoomPosition → Int
x = unsafeField "x"

y ∷ RoomPosition → Int
y = unsafeField "y"

createConstructionSite ∷ RoomPosition → StructureType → Effect ReturnCode
createConstructionSite = runThisEffFn1 "createConstructionSite"

createFlag ∷ RoomPosition → Effect ReturnCode
createFlag = runThisEffFn0 "createFlag"

-- findClosestByPath ∷ ∀ α. RoomPosition → FindContext α → Either Error (Maybe α)
-- findClosestByPath pos ctx = tryPure
--   (toMaybe <$> runThisEffFn1 "findClosestByPath" pos (unwrapContext ctx))
-- 
-- findClosestByPath' ∷ ∀ α. RoomPosition → FindContext α
--   → ClosestPathOptions → Either Error (Maybe α)
-- findClosestByPath' pos ctx opts = tryPure
--   (toMaybe <$> runThisEffFn2 "findClosestByPath" pos ctx' options)
--   where ctx'    = unwrapContext ctx
--         options = selectMaybes  opts

-- findClosestByRange ∷ ∀ α. RoomPosition → FindContext α → Either Error (Maybe α)
-- findClosestByRange pos ctx = tryPure
--   (toMaybe <$> runThisEffFn1 "findClosestByRange" pos (unwrapContext ctx))
-- 
-- findClosestByRange' ∷ ∀ α. RoomPosition → FindContext α
--   → FilterFn α → Either Error (Maybe α)
-- findClosestByRange' pos ctx filter = tryPure
--   (toMaybe <$> runThisEffFn2 "findClosestByRange" pos (unwrapContext ctx) { filter })
-- 
-- findInRange ∷ ∀ α. RoomPosition → FindContext α → Int → Either Error (Array α)
-- findInRange pos ctx range = tryPure
--   (runThisEffFn2 "findInRange" pos (unwrapContext ctx) range)
-- 
-- findInRange' ∷ ∀ α. RoomPosition → FindContext α
--   → Int → FilterFn α → Either Error (Array α)
-- findInRange' pos ctx range filter = tryPure
--   (runThisEffFn3 "findInRange" pos (unwrapContext ctx) range { filter })

getDirectionTo ∷ ∀ α. RoomPosition → TargetPosition α → Direction
getDirectionTo pos (TargetPt x' y') = runThisFn2 "getDirectionTo" pos x' y'
getDirectionTo pos (TargetPos posD) = runThisFn1 "getDirectionTo" pos posD
getDirectionTo pos (TargetObj obj)  = runThisFn1 "getDirectionTo" pos obj

-- may return infinity
getRangeTo ∷ ∀ α. RoomPosition → TargetPosition α → Int
getRangeTo pos (TargetPt x' y') = runThisFn2 "getRangeTo" pos x' y'
getRangeTo pos (TargetPos posD) = runThisFn1 "getRangeTo" pos posD
getRangeTo pos (TargetObj obj)  = runThisFn1 "getRangeTo" pos obj

inRangeTo ∷ ∀ α. RoomPosition → TargetPosition α → Int → Boolean
inRangeTo pos (TargetPt x' y') = runThisFn3 "inRangeTo" pos x' y'
inRangeTo pos (TargetPos posD) = runThisFn2 "inRangeTo" pos posD
inRangeTo pos (TargetObj obj)  = runThisFn2 "inRangeTo" pos obj

isEqualTo ∷ ∀ α. RoomPosition → TargetPosition α → Boolean
isEqualTo pos (TargetPt x' y') = runThisFn2 "isEqualTo" pos x' y'
isEqualTo pos (TargetPos posD) = runThisFn1 "isEqualTo" pos posD
isEqualTo pos (TargetObj obj)  = runThisFn1 "isEqualTo" pos obj

isNearTo ∷ ∀ α. RoomPosition → TargetPosition α → Boolean
isNearTo pos (TargetPt x' y') = runThisFn2 "isNearTo" pos x' y'
isNearTo pos (TargetPos posD) = runThisFn1 "isNearTo" pos posD
isNearTo pos (TargetObj obj)  = runThisFn1 "isNearTo" pos obj

-- TODO: implement look
-- look

-- lookFor ∷ ∀ α. RoomPosition → LookType α → Either Error (Array α)
-- lookFor pos lookType = tryPure (runThisEffFn1 "lookFor" pos lookType)
