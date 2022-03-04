module Screeps.PathFinder where

import UPrelude
import Data.Map as M
import Effect (Effect)
import Effect.Console (log)
import Screeps.RoomObject as RO
import Screeps.RoomPosition as RP
import Screeps.Data
import Screeps.FFI

foreign import getPathFinderGlobal ∷ Effect PathFinder

search ∷ PathFinder → RoomPosition → Array Goal → Effect ReturnPath
search = runThisEffFn2 "search"
searchTarget ∷ ∀ α. String → PathFinder → TargetPosition α → Array Goal → Effect ReturnPath
searchTarget room pf (TargetPt x y)  gs = runThisEffFn2 "search" pf pos gs
  where pos = RP.createRoomPosition x y room
searchTarget room pf (TargetObj obj) gs = runThisEffFn2 "search" pf (RO.pos obj) gs
searchTarget room pf (TargetPos pos) gs = runThisEffFn2 "search" pf pos gs
-- TODO: options
--search' ∷ PathFinder → RoomPosition → Array Goal → PathFindArgs → Effect ReturnPath
--search' = runThisEffFn3 "search"

-- TODO: cost matrix
