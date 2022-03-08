module Creep.Collier where
-- ^ colliers are energy miners

import UPrelude
import Control.Monad.Reader (asks)
import Data.Maybe (Maybe(..))
import Data.Array (uncons)
import Screeps.Game as Game
import Screeps.Room as Room
import Screeps.RoomObject as RO
import Screeps.RoomPosition as RP
import Screeps.Creep as Creep
import Screeps.Data
import Screeps.Const (ok)
import Util (posEqSpot, spotToPos)
import CG
import Data

collierMove ∷ Creep → Path → CG Env Unit
-- either we need a new path or we are at our destination
-- in case of a weird error this might be a problem
collierMove creep []   = do
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
collierMove creep path = case uncons path of
    Nothing              → log' LogError "this error is not possible"
    Just {head:h,tail:t} → do
      ret ← moveCreep creep h.direction
      let cn = Creep.name creep
      if ret ≠ ok then do
        dest ← getCreepMem creep "dest"
        case dest of
          Nothing → log' LogWarn "creep has path but no destination"
          -- we may not need this check but it never hurts
          Just d0 → if (RO.pos creep) `posEqSpot` d0 then
                      setCreepMem creep "harvesting" true
                      -- TODO: remake path here 
                    else pure unit

      else setCreepMem creep "path" t

collierHarvest ∷ Creep → CG Env Unit
collierHarvest creep = do
  target ← getCreepMem creep "target"
  case target of
    Nothing → log' LogError "collier lost its target"
    Just t0 → do
      game       ← asks (_.game)
      source'    ← getObjectById' t0
      case source' of
        Nothing → log' LogError "source destination no longer exists"
        Just s0 → do
          harv ← creepHarvest creep s0
          if harv ≠ ok then
            log' LogWarn $ "collier was unable to harvest: " <> (show harv)
          else pure unit
