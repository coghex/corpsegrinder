module Creep where

import UPrelude
import Effect.Class (liftEffect)
import Data.Array (uncons)
import Data.Maybe (Maybe(..))
import Screeps.Data
import Screeps.Creep as Creep
import Screeps.Room as Room
import Screeps.RoomObject as RO
import Screeps.RoomPosition as RP
import Screeps.Const (ok)
import Util (posEqSpot, spotToPos)
import CG

creepMove ∷ Creep → Path → CG Env Unit
-- either we need a new path or we are at our destination
-- in case of a weird error this might be a problem
creepMove creep []   = do
  dest ← getCreepMem creep "dest"
  -- if we have no destination, then something went very wrong
  case dest of
    Nothing → log' LogWarn "cant move creep without a destination"
    Just d0 → if (RO.pos creep) `posEqSpot` d0 then
                setCreepMem creep "harvesting" true
              else do
                let path = Room.findPath (RO.room creep) (RO.pos creep)
                                         $ spotToPos name d0
                    name = Room.name (RO.room creep)
                setCreepMem creep "path" path
-- | just follow the directions
creepMove creep path = case uncons path of
    Nothing              → log' LogError "this error is not possible"
    Just {head:h,tail:t} → do
      ret ← liftEffect $ Creep.move creep h.direction
      let cn = Creep.name creep
      if ret ≠ ok then do
        --log' LogWarn $ "Creep " <> cn <> " cant move"
        pure unit
      else setCreepMem creep "path" t
