module Role.Builder where
import UPrelude
import Control.Monad.Reader (asks)
import Data.Array (index, length, head)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (JsonDecodeError, getField)
import Screeps.Const ( err_not_in_range, find_sources, find_my_spawns
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
import Util (findNearest, findNearestOpenSource, setNHarvs, removeNHarvs)
import Creep.Peon (getEnergy, creepSpaceForEnergy, creepFull)
import CG

-- | a builder moves between mining for energy and building construction sites
preformBuilder ∷ Creep → CG Env Unit
preformBuilder creep = do
  mem ← getAllCreepMem creep
  case mem of
    Nothing → pure unit
    Just d0 → preformBuilderF creep d0
preformBuilderF ∷ Creep → F.Object Json → CG Env Unit
preformBuilderF creep mem = do
  case (getField mem "building") of
    -- this means its a new builder, so set the memory
    -- accordingly first, then continue
    Left  _        → do
      setCreepMem creep "building" false
      preformBuilderFF creep false
    -- if we are already a builder this will be set already
    Right building → preformBuilderFF creep building
preformBuilderFF ∷ Creep → Boolean → CG Env Unit
preformBuilderFF creep building = if building then do
    if creepSpaceForEnergy creep ≤ 0 then do
      setCreepMem creep "building" false
    else if creepFull creep then do
      setCreepMem creep "building" true
    else pure unit
    let targets = find (RO.room creep) find_construction_sites
    case (findNearest targets (RO.pos creep)) of
      Nothing → pure unit
      Just nearestTarget → if (length targets > 0) then do
        res ← creepBuild creep nearestTarget
        if (res ≡ err_not_in_range) then do
          ret ← moveCreepTo creep (TargetObj nearestTarget)
          pure unit
        else pure unit
   else pure unit
  else getEnergy creep
