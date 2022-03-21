module Preformer where

import UPrelude
import Data.Tuple ( Tuple(..), fst, snd )
import Data.Array ( uncons, head, tail, length )
import Foreign.Object as F
import Screeps.Data
import Role.Harvester ( preformHarvester )
import Role.Upgrader ( preformUpgrader )
import Role.Builder ( preformBuilder )
import Data
import Spawn
import Creep

preformCreeps ∷ Spwn Unit
preformCreeps = do
  creeps' ← getCreeps'
  let creeps = F.toUnfoldable creeps'
  preformCreepsF creeps
preformCreepsF ∷ Array (Tuple String Creep) → Spwn Unit
preformCreepsF []     = pure unit
preformCreepsF creeps = case uncons creeps of
  Nothing              → pure unit
  Just {head:h,tail:t} → do
    m ← getCreepMem' $ snd h
    case m of
      Nothing → log'' LogError "creep lost its memory"
      Just m0 → runCE preformCreep { creep:snd h, mem:m0 }
    preformCreepsF t
preformCreep ∷ Crp Unit
preformCreep = do
  role ← getCreepMemField "role"
  case role of
    Just RoleHarvester  → preformHarvester
    Just RoleUpgrader   → preformUpgrader
    Just RoleBuilder    → preformBuilder
    _                   → pure unit
