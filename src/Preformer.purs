module Preformer where

import UPrelude
import Control.Monad.Reader (asks)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Array (uncons)
import Data.Tuple (Tuple(..))
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (getField)
import Screeps.Data
import Foreign.Object as F
import Screeps.Game as Game
import Screeps.Structure.Spawn as Spawn
import Screeps.Store as Store
import Screeps.Const (resource_energy, pWork, pMove, pCarry)
import Role.Harvester (preformHarvester)
import Role.Collier (preformCollier)
import Role.Upgrader (preformUpgrader)
import Role.Builder (preformBuilder)
import Role.Worker (preformWorker)
import Data
import CG

-- | creeps change their roles here
preformCreeps ∷ CG Env Unit
preformCreeps = do
  creeps' ← getMemField "creeps"
  let creeps = F.toUnfoldable $ case creeps' of
                 Nothing → F.empty
                 Just c0 → c0
  preformCreepsF creeps
preformCreepsF ∷ Array (Tuple String (F.Object Json)) → CG Env Unit
preformCreepsF []     = pure unit
preformCreepsF creeps = do
  preformCreep creep'
  preformCreepsF creeps'
    where creep'  = case uncons creeps of
                      Just {head: c, tail: _}  → c
                      Nothing                  → Tuple "NULL" F.empty
          creeps' = case uncons creeps of
                      Just {head: _, tail: cs} → cs
                      Nothing                  → []

preformCreep ∷ Tuple String (F.Object Json) → CG Env Unit
preformCreep (Tuple "NULL" val) = pure unit
preformCreep (Tuple key    val) = preformRole key role
  where role = case getField val "role" of
                 Left err → RoleNULL
                 Right r0 → r0
preformRole ∷ String → Role → CG Env Unit
preformRole key role = do
  game ← asks (_.game)
  let creep = F.lookup key (Game.creeps game)
  case creep of
    Nothing → pure unit
    Just c0 → preformRoleF c0 role
preformRoleF ∷ Creep → Role → CG Env Unit
preformRoleF creep RoleIdle        = pure unit
preformRoleF creep RoleHarvester   = preformHarvester creep
preformRoleF creep (RoleWorker j)  = preformWorker    creep j
preformRoleF creep (RoleBuilder _) = preformBuilder   creep
preformRoleF creep RoleUpgrader    = preformUpgrader  creep
preformRoleF creep RoleCollier     = preformCollier   creep
preformRoleF creep RoleNULL        = pure unit
