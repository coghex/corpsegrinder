module Main where

import UPrelude
import Effect (Effect)
import Data.Array (length)
import Control.Monad.Reader (asks)
import Control.Monad.Trans.Class (lift)
import Screeps.Room   as Room
import Screeps.RoomObject as RO
import Screeps.Memory as Memory
import Screeps.Game   as Game
import Screeps.Structure.Spawn as Spawn
import Screeps.PathFinder as PF
import Screeps.Const (find_sources)
import Foreign.Object as F
import Memory (freeCreepMemory)
import Manager (manageCreeps, manageJobs, calcMaxCreeps)
import Monitor (monitorCreeps)
import Processor (processCreeps, numberOfRole)
import Preformer (preformCreeps)
import Builder (buildRoom)
import Data (LoopStatus(..), Role(..), Job(..), CreepCounts(..))
import Spawn
import CG

main ∷ Effect Unit
main = do
  m  ← Memory.getMemoryGlobal
  g  ← Game.getGameGlobal
  pf ← PF.getPathFinderGlobal
  let t = Game.time g
  runCG corpseGrinder { memory:m, game:g, pf:pf, time:t }
corpseGrinder ∷ CG Env Unit
corpseGrinder = do
  loopStatus ← getMemField "loopStatus"
  case loopStatus of
      Nothing  → setMemField "loopStatus" LoopStart
      Just ls0 → do
        game ← asks (_.game)
        let spawn1 = F.lookup "Spawn1" spawnsList
            spawnsList = Game.spawns game
        case spawn1 of
          Nothing → pure unit
          Just s1 → runSpawnEnv (runCorpsegrinder ls0) { spawn:s1, mem:mem }
            where mem = Spawn.memory s1
runCorpsegrinder ∷ LoopStatus → Spwn Unit
runCorpsegrinder LoopStart       = do
  log'' LogInfo "starting the corpsegrinder..."
  setMemField' "loopStatus" LoopGo
  setMemField' "utility"    0
  initSpawn
  manageCreeps
runCorpsegrinder LoopGo          = do
  time ← lift $ asks (_.time)
  -- the following functions will get called once every 12 ticks
  case time `mod` 12 of
    -- free memory associated with dead creeps
    0 → freeCreepMemory
    -- prints whatever
    --1 → consoleClear
    --2 → printDebug
    -- monitors changes to the game state and sets memory accordingly
    3 → lift monitorCreeps
    5 → manageCreeps
    6 → lift manageJobs
    9 → lift buildRoom
    _ → pure unit
  lift $ processCreeps time
  lift preformCreeps
runCorpsegrinder LoopReset       = do
  log'' LogInfo $ "resetting the corpsegrinder"
  lift $ clearMem
  setMemField' "loopStatus" LoopGo
  setMemField' "utility"    0
  initSpawn
  manageCreeps
runCorpsegrinder (LoopError str) = log'' LogError $ "Error: " <> str
runCorpsegrinder LoopNULL        = pure unit
runCorpsegrinder ls = pure unit

printDebug ∷ Spwn Unit
printDebug = do
  s1 ← asks (_.spawn)
  creeps' ← getMemField' "creeps"
  let creeps = case creeps' of
                 Nothing → F.empty
                 Just c0 → c0
  let n         = length $ Room.find (RO.room s1) find_sources
  maxCreeps ← lift $ calcMaxCreeps n s1
  let nHarv     = numberOfRole RoleHarvester         creeps
      nUpgrader = numberOfRole RoleUpgrader          creeps
      nWorker   = numberOfRole (RoleWorker  JobNULL) creeps
      nBldr     = numberOfRole (RoleBuilder 0)       creeps
      CreepCounts ({nPeon,nCollier,nHauler,nGrunt}) = maxCreeps
  log'' LogDebug "  ------- creep counts -------"
  log'' LogDebug $ "nPeon: " <> (show nPeon) <> ", nCollier: "  <> (show nCollier)
  log'' LogDebug $ "nGrnt: " <> (show nGrunt) <> ", nHauler: "   <> (show nHauler)
  log'' LogDebug "  -------- job counts --------"
  log'' LogDebug $ "nHarv: " <> (show nHarv) <> ", nUpgrader: " <> (show nUpgrader)
  log'' LogDebug $ "nBldr: " <> (show nBldr) <> ", nWorker: "   <> (show nWorker)
