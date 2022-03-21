module Main where

import UPrelude
import Effect (Effect)
import Data.Array (uncons)
import Foreign.Object          as F
import Screeps.Game            as Game
import Screeps.Memory          as Memory
import Screeps.Structure.Spawn as Spawn
import Screeps.Data
import Monitor ( monitorCreeps )
import Manager ( manageCreeps, manageJobs )
import Builder ( buildRoom )
import Processor ( processCreeps )
import Preformer ( preformCreeps )
import CorpseGrinder
import Memory
import Spawn
import Data

-- | main sets up a reader monad over effect,
-- | inits the environment, then runs the monadic action
main ∷ Effect Unit
main = do
  m  ← Memory.getMemoryGlobal
  g  ← Game.getGameGlobal
  -- certain values like the time are not worth accessing
  -- each time we want it, it is checked each tick anyways
  let t = Game.time g
  runCG corpseGrinder { memory:m, game:g, time:t }
-- | the corpsegrinder sets the status of the loop, then find
-- | a list of spawns, and does work for each one
corpseGrinder ∷ CorpseGrinder Env Unit
corpseGrinder = do
  loopStatus ← getMemField "loopStatus"
  case loopStatus of
    Nothing → do
      setMemField "loopStatus" LoopStart
      spawnsList' ← getSpawns
      let spawnsList = F.toArrayWithKey (\_ a → a) spawnsList'
      manageSpawns spawnsList
    Just LoopReset → do
      -- TODO: clear memory here
      pure unit
    Just LoopStart → do
      log' LogInfo "starting the corpsegrinder..."
      setMemField "loopStatus" LoopGo
      setMemField "utility"    0
      spawnsList' ← getSpawns
      let spawnsList = F.toArrayWithKey (\_ a → a) spawnsList'
      initSpawns spawnsList
    Just LoopGo → do
      time        ← getTime
      case time `mod` 12 of
        0 → freeCreepMemory
        _ → pure unit
      spawnsList' ← getSpawns
      let spawnsList = F.toArrayWithKey (\_ a → a) spawnsList'
      manageSpawns spawnsList
    Just (LoopError str) → log' LogError str
    Just LoopNULL        → log' LogError "LoopNULL"
manageSpawns ∷ Array Spawn → CorpseGrinder Env Unit
manageSpawns []     = pure unit
manageSpawns spawns =
  case uncons spawns of
    Nothing                     → pure unit
    Just {head:s0,tail:spawns'} → do
      let mem = Spawn.memory s0
      runSE manageSpawn { spawn:s0, mem:mem }
      manageSpawns spawns'
manageSpawn ∷ Spwn Unit
manageSpawn = do
  time ← getTime'
  case time `mod` 12 of
    3 → monitorCreeps
    5 → manageCreeps
    6 → manageJobs
    9 → buildRoom
    _ → pure unit
  processCreeps
  preformCreeps
