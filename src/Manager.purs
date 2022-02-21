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
import Screeps.Structure.Controller as Controller
import Screeps.Store as Store
import Screeps.Room as Room
import Screeps.RoomObject as RO
import Screeps.Const (resource_energy, pWork, pMove, pCarry)
import Data

-- | creeps are created here
manageCreeps ∷ GameGlobal → MemoryGlobal → Effect Unit
manageCreeps game memory = do
  let spawnslist      = Game.spawns      game
      creeps          = Game.creeps      game
      gcl             = Game.gcl         game
      spawn1          = F.lookup "Spawn1" spawnslist
  case spawn1 of
    -- there is no spawn yet, so just repeat this check here
    Nothing → Memory.set memory "loopStatus" LoopStart
    Just s1 → case rcl of
      1 → do
        if availableEnergy > 250 && numCreeps < 3 then do
                    log "creating level 1 creep..."
                    createCreep s1 1
                  else pure unit
          where availableEnergy = Store.getUsedCapacity' spawnStore resource_energy
                spawnStore      = Spawn.store            s1
                numCreeps       = F.size                 creeps
      _ → pure unit
      where room1           = RO.room                s1
            controller1     = Room.controller        room1
            rcl             = Controller.level       controller1


-- | basic creep creation function
createCreep ∷ Spawn → Int → Effect Unit
createCreep spawn 1 = do
    spawnCreep spawn [pWork,pCarry,pMove,pMove] RoleIdle CreepPeon
createCreep _     _ = pure unit

-- | pattern match helper function
spawnCreep ∷ Spawn → Array BodyPartType
  → Role → CreepType → Effect Unit
spawnCreep spawn parts r t = do
    res ← Spawn.spawnCreep' spawn parts Nothing { typ: t, role: r, utility: 0 }
    case res of
        Left  err → log $ show err
        Right str → log $ str <> " created succesfully"
