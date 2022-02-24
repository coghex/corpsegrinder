module Preformer where

import UPrelude
import Effect (Effect)
import Effect.Console (log)
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
import Screeps.Memory as Memory
import Screeps.Structure.Spawn as Spawn
import Screeps.Store as Store
import Screeps.Const (resource_energy, pWork, pMove, pCarry)
import Role.Harvester (preformHarvester)
import Role.Builder (preformBuilder)
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
preformCreep (Tuple key    val) = case role of
  RoleNULL → pure unit
  RoleIdle → pure unit
  RoleBuilder _ → do
    game ← asks (_.game)
    let creep = F.lookup key (Game.creeps game)
    case creep of
      Nothing → pure unit
      Just c0 → preformBuilder c0
  RoleHarvester → do
    game ← asks (_.game)
    let creep = F.lookup key (Game.creeps game)
    case creep of
      Nothing → pure unit
      Just c0 → preformHarvester c0
  where role = case getField val "role" of
                 Left err → RoleNULL
                 Right r0 → r0
