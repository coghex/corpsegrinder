module Memory where

import UPrelude
import Control.Monad.Reader (asks)
import Data.Array (uncons, head, tail)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (JsonDecodeError, getField)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Screeps.Data
import Foreign.Object as F
import Screeps.Creep as Creep
import Screeps.Game as Game
import Screeps.Memory as Memory
import Data (CreepType, Role, HarvestSpot)
import Util ( removeNHarvs )
import CG

-- | called every once in a while to get rid of memory of dead creeps
--   and set corresponding values in AI state to compensate
freeCreepMemory ∷ CG Env Unit
freeCreepMemory = do
  creeps' ← getMemField "creeps"
  let creeps = case creeps' of
                 Nothing → []
                 Just c0 → F.keys c0
  case creeps' of
    Nothing    → log' LogWarn "no creeps in memory"
    Just cObjs → freeCreepMemoryF cObjs creeps
freeCreepMemoryF ∷ F.Object (F.Object Json) → Array String → CG Env Unit
freeCreepMemoryF _   []     = pure unit
freeCreepMemoryF mem creeps = do
  game ← asks (_.game)
  let creepList = Game.creeps game
  if creepN `F.member` creepList then freeCreepMemoryF mem creeps'
  else do
    log' LogDebug $ "freeing creep " <> creepN <> "..."
    let memData = F.lookup creepN mem
    case memData of
      Nothing → pure unit
      Just md → case (getField md "target") of
                  Left err → log' LogError $ show err
                  Right t0 → case (getField md "home") of
                    Left err → log' LogError $ show err
                    Right h0 → freeSpawnNHarv t0 h0
    memory ← asks (_.memory)
    freeCreepMem creepN
    freeCreepMemoryF mem creeps'
  where creeps' = case uncons creeps of
                    Just {head: _, tail: cs} → cs
                    Nothing                  → []
        creepN  = case uncons creeps of
                    Just {head: c, tail: _}  → c
                    Nothing                  → ""

-- | removes creeps nharv value on a source when it dies
freeSpawnNHarv ∷ Id Source → Id Spawn → CG Env Unit
freeSpawnNHarv destid spawnid = do
  game ← asks (_.game)
  let spawn' = Game.getObjectById game spawnid
  case spawn' of
    Nothing → log' LogWarn "spawn has no memory"
    Just spawn → do
      harvSpots' ← getSpawnMem spawn "harvestSpots"
      case harvSpots' of
        Nothing → log' LogWarn "destination not a spawn"
        Just harvSpots → removeNHarvs harvSpots destid spawn


