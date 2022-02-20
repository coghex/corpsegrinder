module Manager where

import UPrelude
import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Screeps.Data
import Foreign.Object as F
import Screeps.Game as Game
import Screeps.Memory as Memory
import Screeps.Structure.Spawn as Spawn
import Screeps.Store as Store
import Screeps.Const (resource_energy, pWork, pMove, pCarry)
import Data

manageCreeps ∷ F.Object Creep → GameGlobal → MemoryGlobal → Effect Unit
manageCreeps creeps game memory = do
  let spawnslist      = Game.spawns game
      spawn1          = F.lookup "Spawn1" spawnslist
  case spawn1 of
    -- there is no spawn yet, so just repeat this check here
    Nothing → Memory.set memory "loopStatus" LoopStart
    Just s1 → if availableEnergy > 250 && numCreeps < 3 then do
    -- name "Creep1" here is currently just a placeholder
                log "creating level 1 creep..."
                createCreep s1 1
              else pure unit
      where availableEnergy = Store.getUsedCapacity' spawnStore resource_energy
            spawnStore      = Spawn.store s1
            numCreeps       = F.size creeps

-- | basic creep creation function
createCreep ∷ Spawn → Int → Effect Unit
createCreep spawn 1 = do
    spawnCreep spawn [pWork,pCarry,pMove,pMove] RoleIdle
createCreep spawn level = do
    spawnCreep spawn [pWork,pWork,pCarry,pMove] RoleIdle

-- | pattern match helper function
spawnCreep ∷ Spawn → Array BodyPartType
  → Role → Effect Unit
spawnCreep spawn parts r = do
    res ← Spawn.spawnCreep' spawn parts Nothing { role: r, utility: 0 }
    case res of
        Left  err → log $ show err
        Right str → log $ str <> " created succesfully"
