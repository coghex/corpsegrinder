module Role.Janitor where
import UPrelude
import Control.Monad.Reader (asks)
import Data.Array (index, length, head)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (JsonDecodeError, getField)
import Screeps.Const ( err_not_in_range, find_my_structures, find_my_spawns
                     , find_construction_sites, resource_energy
                     , structure_spawn, structure_tower
                     , structure_extension )
import Screeps.Data
import Screeps.Creep as Creep
import Screeps.Room ( find, find' )
import Screeps.RoomObject as RO
import Screeps.Game as Game
import Screeps.Store as Store
import Screeps.Source as Source
import Screeps.Structure ( structureType )
import Foreign.Object as F
import Util (findNearest, needsRepair)
import Creep.Peon (getEnergy, creepSpaceForEnergy, creepFull
                  , creepEmpty, creepHasEnergy)
import CG

-- | a builder moves between mining for energy and building construction sites
preformJanitor ∷ Creep → CG Env Unit
preformJanitor creep = do
  mem ← getAllCreepMem creep
  case mem of
    Nothing → pure unit
    Just d0 → preformJanitorF creep d0
preformJanitorF ∷ Creep → F.Object Json → CG Env Unit
preformJanitorF creep mem = do
  case (getField mem "janiting") of
    -- this means its a new builder, so set the memory
    -- accordingly first, then continue
    Left  _        → do
      setCreepMem creep "janiting" false
      preformJanitorFF creep false
    -- if we are already a builder this will be set already
    Right building → preformJanitorFF creep building
preformJanitorFF ∷ Creep → Boolean → CG Env Unit
preformJanitorFF creep building =
  if building then do
    if creepEmpty creep then do
      setCreepMem creep "janiting" false
    else if creepHasEnergy creep then do
      let targets = find' (RO.room creep) find_my_structures needsRepair
      case (findNearest targets (RO.pos creep)) of
        Nothing → pure unit
        Just nearestTarget → if (length targets > 0) then do
          res ← creepRepair creep nearestTarget
          if (res ≡ err_not_in_range) then do
            ret ← moveCreepTo creep (TargetObj nearestTarget)
            pure unit
          else pure unit
        else pure unit
    else pure unit
  else if creepFull creep then setCreepMem creep "janiting" true
  else getEnergy creep
