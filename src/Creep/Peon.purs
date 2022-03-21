module Creep.Peon where
import UPrelude
import Control.Monad.Reader ( asks )
import Control.Monad.Trans.Class ( lift )
import Data.Array ( uncons, length )
import Screeps.Source     as Source
import Screeps.Creep      as Creep
import Screeps.Game       as Game
import Screeps.Room       as Room
import Screeps.RoomObject as RO
import Screeps.Data
import Screeps.Const ( find_structures, find_sources
                     , find_my_spawns, ok, err_not_in_range
                     , resource_energy )
import Creep.Util
import Util ( spotToPos, posToSpot, posEqSpot, findOpenSource
            , setNHarvs, hasFreeSpace, findNearest )
import Spawn ( getSpawnMem' )
import Creep
import CorpseGrinder ( getObjectById', randomNumber )
import Data

-- | moves a peon type creep to a room position
peonMove ∷ RoomPosition → Crp Unit
peonMove dest = do
  pathMem ← getCreepMemField "path"
  path0 ← case pathMem of
    Nothing → do
      creep ← asks (_.creep)
      let path     = Room.findPath (RO.room creep) (RO.pos creep) dest
          destSpot = posToSpot dest
      setCreepMemField' "path" path
      setCreepMemField' "dest" destSpot
      pure path
    Just [] → do
      -- clear and reset pathing when at paths' end
      creep ← asks (_.creep)
      dest ← findAndSetDestAndTarget RoleHarvester
      let path     = Room.findPath (RO.room creep) (RO.pos creep) dest
      setCreepMemField' "path" path
      pure path
    Just p0 → pure p0
  peonMoveAlongPath path0
peonMoveAlongPath ∷ Path → Crp Unit
peonMoveAlongPath []   = do
  dest' ← getCreepMemField "dest"
  case dest' of
    Nothing → log''' LogWarn "creep has no dest"
    Just d0 → do
      creep ← asks (_.creep)
      if (RO.pos creep) `posEqSpot` d0 then
        setCreepMemField' "moving" false
      else do
        let path     = Room.findPath room creepPos d0'
            name     = Room.name room
            d0'      = spotToPos name d0
            room     = RO.room creep
            creepPos = RO.pos  creep
        setCreepMemField' "path" path
peonMoveAlongPath path = case uncons path of
  Nothing → log''' LogError "this error is not possible"
  Just {head:h,tail:[]} → do
    setCreepMemField' "moving" false
    setCreepMemField' "path"   ([] ∷ Path)
  Just {head:h,tail:t}  → do
    ret ← moveCreep' h.direction
    creep ← asks (_.creep)
    let cn = Creep.name creep
    if ret ≠ ok then do
      dest ← getCreepMemField "dest"
      cPos ← creepPos
      case dest of
        Nothing → log''' LogWarn "creep has path but no dest"
        Just d0 → if cPos `posEqSpot` d0 then
                    setCreepMemField' "moving" false
                  else pure unit
    else setCreepMemField' "path" t

-- | all peons will need to harvest their own energy
peonHarvest ∷ Crp Unit
peonHarvest = do
  target ← getCreepMemField "target"
  case target of
    Nothing → do
      dest ← findAndSetDestAndTarget RoleHarvester
      pure unit
      --log''' LogError "peon lost its harvest target"
    Just t0 → do
      source' ← lift $ lift $ getObjectById' t0
      case source' of
        Nothing → log''' LogError "source destination no longer exists"
        Just s0 → do
          harv ← creepHarvest' s0
          cFull ← creepFull
          if harv ≠ ok then do
            setCreepMemField' "moving"     true
          else if cFull then do
            setCreepMemField' "harvesting" false
            setCreepMemField' "moving"     true
            dest ← findAndSetDestAndTarget RoleHarvester
            peonMove dest
          else pure unit

-- | checks if we were already going somewhere and if not
--   finds a new source and resets dest
getEnergy ∷ Crp Unit
getEnergy = do
  dest ← getCreepMemField "target"
  case dest of
    Nothing → do
      home ← getCreepMemField "home"
      game ← lift $ lift $ asks (_.game)
      case home of
        Nothing → pure unit
        Just h0 → case (Game.getObjectById game h0) of
          Nothing → pure unit
          Just s0 → do
            let sources = Room.find room find_sources
                spawns  = Room.find room find_my_spawns
                room    = RO.room s0
            ret ← lift $ getSpawnMem' "harvestSpots"
            let harvSs = case ret of
                         Nothing → []
                         Just h0 → h0
            -- set to a pseudo-random target
            rand ← lift $ lift $ randomNumber 0
            case (findOpenSource harvSs sources) of
              Nothing  → pure unit
              Just sid → do
                setCreepMemField' "target" (Source.id sid)
                lift $ lift $ setNHarvs harvSs (Source.id sid) s0
    Just d0 → do
      game ← lift $ lift $ asks (_.game)
      creep ← asks (_.creep)
      let nearestSource' = Game.getObjectById game d0
      case nearestSource' of
        Nothing → log''' LogWarn $ "creep " <> (Creep.name creep)
                                            <> " has lost its destination: "
                                            <> (show d0)
        Just nearestSource → do
          harv ← creepHarvest' nearestSource
          if harv ≡ err_not_in_range then do
            ret ← moveCreepTo' (TargetObj nearestSource)
            pure unit
          else pure unit
storeEnergy ∷ Crp Unit
storeEnergy = do
  creep ← asks (_.creep)
  let targets = Room.find' (RO.room creep) find_structures hasFreeSpace
  case (findNearest targets (RO.pos creep)) of
    Nothing → pure unit
    Just nearestTarget → if (length targets) > 0 then do
           targ ← transferResourceTo' nearestTarget resource_energy
           if targ ≡ err_not_in_range then do
             ret ← moveCreepTo' (TargetObj nearestTarget)
             pure unit
           else pure unit
         else pure unit

-- | trys to dropoff energy
peonDeposit ∷ Crp Unit
peonDeposit = do
  target' ← getCreepMemField "target"
  case target' of
    Nothing  → log''' LogWarn "creep has lost deposit target"
    Just tid → do
      obj ← lift $ lift $ getObjectById' tid
      case obj of
        Nothing   → log''' LogWarn "creep has lost its deposit target id"
        Just tObj → do
          targ  ← transferResourceTo' tObj resource_energy
          if targ ≠ ok then do
            --log''' LogError $ "target error: " <> (show targ)
            setCreepMemField' "moving"     true
            dest ← findAndSetDestAndTarget RoleHarvester
            peonMove dest
          else do
            setCreepMemField' "harvesting" true
            dest ← findAndSetDestAndTarget RoleHarvester
            peonMove dest
