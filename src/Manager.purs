module Manager where

import UPrelude
import Effect (Effect)
import Effect.Console (log)
import Effect.Class (liftEffect)
import Data.Array (foldr)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Control.Monad.Reader (ask)
import Screeps.Data
import Foreign.Object as F
import Screeps.Game as Game
import Screeps.Memory as Memory
import Screeps.Structure as Structure
import Screeps.Structure.Spawn as Spawn
import Screeps.Structure.Controller as Controller
import Screeps.Store as Store
import Screeps.Room as Room
import Screeps.RoomObject as RO
import Screeps.Const (resource_energy, pWork, pMove, pCarry)
import Data
import CG

-- | creeps are created here
manageCreeps ∷ CG Env Unit
manageCreeps = do
  {memory:memory, game:game} ← ask
  let spawnslist      = Game.spawns      game
      creeps          = Game.creeps      game
      gcl             = Game.gcl         game
      spawn1          = F.lookup "Spawn1" spawnslist
  case spawn1 of
    -- there is no spawn yet, so just repeat this check here
    Nothing → setMemField "loopStatus" LoopStart
    Just s1 → case rcl of
      1 → do
        nMaxCreeps ← calcMaxCreeps 1 s1
        if availableEnergy > 250 && numCreeps < nMaxCreeps then do
                    log' LogDebug "creating level 1 creep..."
                    createCreep s1 1
                  else pure unit
          where availableEnergy = Store.getUsedCapacity' spawnStore resource_energy
                spawnStore      = Spawn.store            s1
                numCreeps       = F.size                 creeps
      _ → pure unit
      where room1           = RO.room                s1
            controller1     = Room.controller        room1
            rcl             = Controller.level       controller1

-- | finds the maximum number of creeps at different levels
calcMaxCreeps ∷ Int → Spawn → CG Env Int
calcMaxCreeps 1 spawn = do
  ret ← liftEffect $ Spawn.getMemory spawn "harvestSpots"
  case ret of
    Left  _  → pure 0
    Right h0 → pure $ foldr (addHarvestSpots) 0 h0
calcMaxCreeps _ _     = pure 0
addHarvestSpots ∷ HarvestSpot → Int → Int
addHarvestSpots (HarvestSpot {sourceName, nHarvs, nMaxHarvs, harvSpots}) n
  = n + nMaxHarvs

-- | basic creep creation function
createCreep ∷ Spawn → Int → CG Env Unit
createCreep spawn 1 = do
    spawnCreep spawn [pWork,pCarry,pMove,pMove] RoleIdle CreepPeon
createCreep _     _ = pure unit

-- | pattern match helper function
spawnCreep ∷ Spawn → Array BodyPartType
  → Role → CreepType → CG Env Unit
spawnCreep spawn parts r t = do
    let h = Structure.id spawn
    res ← liftEffect $ Spawn.spawnCreep' spawn parts Nothing { typ: t, role: r, home: h, utility: 0 }
    case res of
        Left  err → log' LogDebug $ show err
        Right str → log' LogDebug $ str <> " created succesfully"
