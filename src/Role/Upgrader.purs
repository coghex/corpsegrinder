module Role.Upgrader where
import UPrelude
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (getField)
import Control.Monad.Trans.Class (lift)
import Screeps.Data
import Screeps.RoomObject as RO
import Screeps.Store as Store
import Screeps.Room as Room
import Screeps.Const ( resource_energy, err_not_in_range )
import Creep.Peon ( getEnergy, storeEnergy, peonMove, peonHarvest )
import Creep.Util
import Foreign.Object as F
import Data ( Role(..) )
import Util ( spotToPos )
import Creep
import CG

-- | a harvester moves between energy source and extension, spawn, or tower
--   the simplest role
preformUpgrader ∷ Creep → CE CreepEnv (CG Env) Unit
preformUpgrader creep = do
  mem ← lift $ getAllCreepMem creep
  case mem of
    Nothing → pure unit
    Just d0 → lift $ preformUpgraderF creep d0
preformUpgraderF ∷ Creep → F.Object Json → CG Env Unit
preformUpgraderF creep mem = do
  moving ← case (getField' mem "moving") of
    Nothing → do
      setCreepMem creep "moving" true
      pure true
    Just m0 → pure m0
  if moving then do
    dest' ← getCreepMem creep "dest"
    case dest' of
      Nothing → do
        dest ← findAndSetDestAndTarget RoleUpgrader creep
        peonMove creep dest
      Just dest → peonMove creep $ spotToPos room dest
        where room = Room.name (RO.room creep)
  else case (getField' mem "upgrading") of
    Nothing → do
      setCreepMem creep "upgrading" false
      preformUpgraderFF creep false
    Just building → preformUpgraderFF creep building
preformUpgraderFF ∷ Creep → Boolean → CG Env Unit
preformUpgraderFF creep true  =
  if creepEmpty creep then do
    setCreepMem creep "upgrading" false
    setCreepMem creep "moving"    true
    dest ← findAndSetDestAndTarget RoleUpgrader creep
    peonMove creep dest
  else do
    let controller = Room.controller (RO.room creep)
    res ← creepUpgrade creep controller
    pure unit
preformUpgraderFF creep false =
  if creepFull creep then do
    setCreepMem creep "upgrading" true
    setCreepMem creep "moving"    true
    dest ← findAndSetDestAndTarget RoleUpgrader creep
    peonMove creep dest
  else peonHarvest creep
