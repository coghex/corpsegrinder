module Role.Upgrader where
import UPrelude
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (getField)
import Screeps.Data
import Screeps.RoomObject as RO
import Screeps.Store as Store
import Screeps.Room as Room
import Screeps.Const (resource_energy, err_not_in_range)
import Creep.Peon (getEnergy, storeEnergy, creepFull, creepEmpty
                  , creepHasEnergy, creepSpaceForEnergy)
import Foreign.Object as F
import CG

-- | a harvester moves between energy source and extension, spawn, or tower
--   the simplest role
preformUpgrader ∷ Creep → CG Env Unit
preformUpgrader creep = do
  mem ← getAllCreepMem creep
  case mem of
    Nothing → pure unit
    Just d0 → preformUpgraderF creep d0
preformUpgraderF ∷ Creep → F.Object Json → CG Env Unit
preformUpgraderF creep mem = do
  case (getField mem "upgrading") of
    Left _ → do
      setCreepMem creep "upgrading" false
      preformUpgraderFF creep false
    Right building → preformUpgraderFF creep building
preformUpgraderFF ∷ Creep → Boolean → CG Env Unit
preformUpgraderFF creep upgrading = do
  if creepEmpty creep then setCreepMem creep "upgrading" false
  else if creepFull creep then setCreepMem creep "upgrading" true
  else pure unit
  if creepHasEnergy creep then do
    let controller = Room.controller (RO.room creep)
    res ← creepUpgrade creep controller
    if (res ≡ err_not_in_range) then do
      ret ← moveCreepTo creep (TargetObj controller)
      pure unit
    else pure unit
  else getEnergy creep
