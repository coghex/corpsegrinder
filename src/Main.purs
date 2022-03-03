module Main where

import UPrelude
import Effect (Effect)
import Control.Monad.Reader (asks)
import Data.Array (length)
import Data.Maybe (Maybe(..))
import Screeps.Room   as Room
import Screeps.RoomObject as RO
import Screeps.Memory as Memory
import Screeps.Game   as Game
import Screeps.Const (find_sources)
import Foreign.Object as F
import Memory (freeCreepMemory)
import Manager (manageCreeps, manageJobs, calcMaxCreeps)
import Processor (processCreeps, numberOfRole)
import Preformer (preformCreeps)
import Builder (buildRoom)
import Spawn (initSpawn)
import Data (LoopStatus(..), Role(..), Job(..), CreepCounts(..))
import CG (CG, Env, LogLevel(..), getMemField, log'
          , runCG, setMemField, clearMem, consoleClear)

main ∷ Effect Unit
main = do
  m ← Memory.getMemoryGlobal
  g ← Game.getGameGlobal
  runCG corpseGrinder { memory:m, game:g }
corpseGrinder ∷ CG Env Unit
corpseGrinder = do
  loopStatus ← getMemField "loopStatus"
  case loopStatus of
      Nothing  → setMemField "loopStatus" LoopStart
      Just ls0 → runCorpsegrinder ls0
runCorpsegrinder ∷ LoopStatus → CG Env Unit
runCorpsegrinder LoopStart       = do
  log' LogInfo "starting the corpsegrinder..."
  setMemField "loopStatus" LoopGo
  setMemField "utility"    0
  initSpawn
  manageCreeps
runCorpsegrinder LoopGo          = do
  game ← asks (_.game)
  let time = Game.time game
      modT = time `mod` 12
  -- the following functions will get called once every 12 ticks
  case modT of
    -- free memory associated with dead creeps
    0 → freeCreepMemory
    -- prints whatever
    1 → consoleClear
    2 → printDebug
    -- create new creeps, manage creep limit
    3 → manageCreeps
    -- create new jobs, place them in job pool
    6 → manageJobs
    -- create new construction sites, based on rcl
    9 → buildRoom
    _ → pure unit
  -- change creep role one per tick
  processCreeps time
  -- preform actions for every creep
  preformCreeps
runCorpsegrinder LoopReset       = do
  log' LogInfo $ "resetting the corpsegrinder..."
  clearMem
  setMemField "loopStatus" LoopGo
  setMemField "utility"    0
  initSpawn
  manageCreeps
runCorpsegrinder (LoopError str) = log' LogError $ "Error: " <> str
runCorpsegrinder LoopNULL        = pure unit

printDebug ∷ CG Env Unit
printDebug = do
  game ← asks (_.game)
  let spawns = Game.spawns game
  case (F.lookup "Spawn1" spawns) of
    Nothing → log' LogError "no spawn 1"
    Just s1 → do
      creeps' ← getMemField "creeps"
      let creeps = case creeps' of
                     Nothing → F.empty
                     Just c0 → c0
      let n         = length $ Room.find (RO.room s1) find_sources
      maxCreeps ← calcMaxCreeps n s1
      let nHarv     = numberOfRole RoleHarvester         creeps
          nUpgrader = numberOfRole RoleUpgrader          creeps
          nWorker   = numberOfRole (RoleWorker  JobNULL) creeps
          nBldr     = numberOfRole (RoleBuilder 0)       creeps
          CreepCounts ({nPeon,nCollier,nHauler,nGrunt}) = maxCreeps
      log' LogDebug "  ------- creep counts -------"
      log' LogDebug $ "nPeon: " <> (show nPeon) <> ", nCollier: "  <> (show nCollier)
      log' LogDebug $ "nGrnt: " <> (show nGrunt) <> ", nHauler: "   <> (show nHauler)
      log' LogDebug "  -------- job counts --------"
      log' LogDebug $ "nHarv: " <> (show nHarv) <> ", nUpgrader: " <> (show nUpgrader)
      log' LogDebug $ "nBldr: " <> (show nBldr) <> ", nWorker: "   <> (show nWorker)


