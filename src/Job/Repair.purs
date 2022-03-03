module Job.Repair where

import UPrelude
import Data.Argonaut.Core (Json)
import Data.Array (length, filter, uncons)
import Data.Maybe (Maybe(..))
import Foreign.Object as F
import Screeps.Data
import Screeps.Structure as Structure
import Screeps.Const (err_not_in_range, find_my_structures)
import Screeps.RoomObject as RO
import Screeps.Room as Room
import Data
import CG
import Util (needsRepair)
import Maths (subtractSet)
import Creep.Peon (getEnergy, creepFull, creepHasEnergy, creepEmpty)

-- | creates new repair jobs
manageRepairJobs ∷ Spawn → CG Env Unit
manageRepairJobs spawn = do
  let room    = RO.room spawn
      needRep = Room.find' room find_my_structures needsRepair
  case length needRep of
    0 → pure unit
    n → do
      jobsTaken ← getMemField "repairJobsTaken"
      case jobsTaken of
        -- if no jobs are taken, set jobs avail to all available repair jobs
        Nothing → setMemField "repairJobsAvail" $ makeRepairJobs needRep
        -- otherwise jobs are sorted
        Just jt → do
          let jobsNotTaken = jobs `subtractSet` jt
              jobs         = makeRepairJobs needRep
          setMemField "repairJobsAvail" jobsNotTaken
makeRepairJobs ∷ ∀ α. Array (Structure α) → Array Job
makeRepairJobs []      = []
makeRepairJobs structs = struct
                       <> makeRepairJobs structs'
  where struct   = case uncons structs of
                     -- this should never be called by definition
                     Nothing              → []
                     Just {head:s,tail:_} → [JobRepair s0]
                       where Id s0 = Structure.id s
        structs' = case uncons structs of
                     Nothing              → []
                     Just {head:_,tail:t} → t

-- | preforms repair jobs
preformJobRepair ∷ ∀ α. Creep → Id (Structure α) → CG Env Unit
preformJobRepair creep struct = do
  mem ← getAllCreepMem creep
  case mem of
    Nothing → pure unit
    Just d0 → preformJobRepairF creep struct d0
preformJobRepairF ∷ ∀ α. Creep → Id (Structure α) → F.Object Json → CG Env Unit
preformJobRepairF creep struct mem = do
  case (getField' mem "repairing") of
    Nothing → do
      setCreepMem creep "repairing" false
      preformJobRepairFF creep struct false
    Just r0 → do preformJobRepairFF creep struct r0
preformJobRepairFF ∷ ∀ α. Creep → Id (Structure α) → Boolean → CG Env Unit
preformJobRepairFF creep struct repairing =
  if repairing then do
    if creepEmpty creep then do
      setCreepMem creep "repairing" false
    else if creepHasEnergy creep then do
      -- target should exist no matter what
      target' ← getObjectById' struct 
      case target' of
        Nothing  → pure unit
        Just target → do
          let hpLeft = Structure.hits    target
              hpTotl = Structure.hitsMax target
          -- job complete when hp almost full
          if (hpTotl - hpLeft) < 2 then setCreepMem creep "role" $ JobNULL
          else do
            res ← creepRepair creep target
            if (res ≡ err_not_in_range) then do
              ret ← moveCreepTo creep (TargetObj target)
              pure unit
            else pure unit
    else pure unit
  else if creepFull creep then setCreepMem creep "repairing" true
  else getEnergy creep

