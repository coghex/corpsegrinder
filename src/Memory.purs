module Memory where

import UPrelude
import Effect (Effect)
import Effect.Console (log)
import Data.Array (uncons, head, tail)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (JsonDecodeError, getField)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Screeps.Data
import Foreign.Object as F
import Screeps.Creep as Creep
import Screeps.Game as Game
import Screeps.Structure.Spawn as Spawn
import Screeps.Memory as Memory
import Data (CreepType, Role, HarvestSpot)
import Util ( removeNHarvs )

-- | called every once in a while to get rid of memory of dead creeps
--   and set corresponding values in AI state to compensate
freeCreepMemory ∷ GameGlobal → MemoryGlobal → Effect Unit
freeCreepMemory game memory = do
  creeps' ← Memory.get memory "creeps"
  let creeps = case creeps' of
                 Left  _  → []
                 Right c0 → F.keys c0
  case creeps' of
    Left  err   → log "no creeps in memory"
    Right cObjs → freeCreepMemoryF game memory cObjs creeps
freeCreepMemoryF ∷ GameGlobal → MemoryGlobal → F.Object (F.Object Json)
  → Array String → Effect Unit
freeCreepMemoryF _    _      _   []     = pure unit
freeCreepMemoryF game memory mem creeps = do
  let creepList = Game.creeps game
  if creepN `F.member` creepList then freeCreepMemoryF game memory mem creeps'
  else do
    log $ "freeing creep " <> creepN <> "..."
    let memData = F.lookup creepN mem
    case memData of
      Nothing → pure unit
      Just md → case (getField md "target") of
                  Left err → log $ show err
                  Right t0 → case (getField md "home") of
                    Left err → log $ show err
                    Right h0 → freeSpawnNHarv game t0 h0
    Memory.freeCreep memory creepN
    freeCreepMemoryF game memory mem creeps'
  where creeps' = case uncons creeps of
                    Just {head: _, tail: cs} → cs
                    Nothing                  → []
        creepN  = case uncons creeps of
                    Just {head: c, tail: _}  → c
                    Nothing                  → ""

freeSpawnNHarv ∷ GameGlobal → Id Source → Id Spawn → Effect Unit
freeSpawnNHarv game destid spawnid = do
  let spawn' = Game.getObjectById game spawnid
  case spawn' of
    Nothing → log "spawn has no memory"
    Just spawn → do
      harvSpots' ← Spawn.getMemory spawn "harvestSpots"
      case harvSpots' of
        Left err → log "destination not a spawn"
        Right harvSpots → removeNHarvs harvSpots destid spawn
