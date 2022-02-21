module Preformer where

import UPrelude
import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Array (uncons)
import Data.Tuple (Tuple(..))
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (getField)
import Screeps.Data
import Foreign.Object as F
import Screeps.Game as Game
import Screeps.Memory as Memory
import Screeps.Structure.Spawn as Spawn
import Screeps.Store as Store
import Screeps.Const (resource_energy, pWork, pMove, pCarry)
import Data
import Role.Harvester (preformHarvester)
import Role.Builder (preformBuilder)

-- | creeps change their roles here
preformCreeps ∷ GameGlobal → MemoryGlobal → Effect Unit
preformCreeps game memory = do
  creeps' ← Memory.get memory "creeps"
  let creeps = F.toUnfoldable $ case creeps' of
                 Left err → F.empty
                 Right c0 → c0
  preformCreepsF game creeps
preformCreepsF ∷ GameGlobal → Array (Tuple String (F.Object Json)) → Effect Unit
preformCreepsF _    []     = pure unit
preformCreepsF game creeps = do
  preformCreep game creep'
  preformCreepsF game creeps'
    where creep'  = case uncons creeps of
                      Just {head: c, tail: _}  → c
                      Nothing                  → Tuple "NULL" F.empty
          creeps' = case uncons creeps of
                      Just {head: _, tail: cs} → cs
                      Nothing                  → []

preformCreep ∷ GameGlobal → Tuple String (F.Object Json) → Effect Unit
preformCreep _    (Tuple "NULL" val) = pure unit
preformCreep game (Tuple key    val) = case role of
  RoleNULL → pure unit
  RoleIdle → pure unit
  RoleBuilder → do
    let creep = F.lookup key (Game.creeps game)
    case creep of
      Nothing → pure unit
      Just c0 → preformBuilder c0
  RoleHarvester → do
    let creep = F.lookup key (Game.creeps game)
    case creep of
      Nothing → pure unit
      Just c0 → preformHarvester c0
  where role = case getField val "role" of
                 Left err → RoleNULL
                 Right r0 → r0
