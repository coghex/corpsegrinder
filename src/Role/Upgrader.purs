module Role.Upgrader where
import UPrelude
import Control.Monad.Trans.Class ( lift )
import Screeps.Room       as Room
import Screeps.RoomObject as RO
import Util ( spotToPos )
import Data ( Role(..) )
import Creep.Util ( findAndSetDestAndTarget, creepEmpty, creepFull )
import Creep.Peon
import Creep
import CorpseGrinder ( getObjectById' )

-- | upgrader is an early RCL unit that upgrades the controller
preformUpgrader ∷ Crp Unit
preformUpgrader = do
  moving' ← getCreepMemField "moving"
  moving ← case moving' of
    Nothing → do
      setCreepMemField' "moving" true
      pure true
    Just m0 → pure m0
  if moving then do
    dest' ← getCreepMemField "dest"
    case dest' of
      Nothing → do
        dest ← findAndSetDestAndTarget RoleUpgrader
        peonMove dest
      Just dest → do
        home' ← getCreepMemField "home"
        case home' of
          Nothing     → pure unit
          Just homeid → do
            home'' ← lift $ lift $ getObjectById' homeid
            case home'' of
              Nothing   → pure unit
              Just home → do
                peonMove $ spotToPos room dest
                  where room = Room.name (RO.room home)
  else do
    upgrading ← getCreepMemField "upgrading"
    case upgrading of
      Nothing → do
        setCreepMemField' "upgrading" false
        preformUpgraderF false
      Just upgrading → preformUpgraderF upgrading
preformUpgraderF ∷ Boolean → Crp Unit
preformUpgraderF true  = do
  cEmpty ← creepEmpty
  if cEmpty then do
    setCreepMemField' "upgrading" false
    setCreepMemField' "moving"    true
    dest ← findAndSetDestAndTarget RoleUpgrader
    peonMove dest
  else do
    home' ← getCreepMemField "home"
    case home' of
      Nothing     → pure unit
      Just homeid → do
        home'' ← lift $ lift $ getObjectById' homeid
        case home'' of
          Nothing   → pure unit
          Just home → do
            res ← creepUpgrade' controller
            pure unit
              where controller = Room.controller (RO.room home)
preformUpgraderF false = do
  cFull ← creepFull
  if cFull then do
    setCreepMemField' "upgrading" true
    setCreepMemField' "moving"    true
    dest ← findAndSetDestAndTarget RoleUpgrader
    peonMove dest
  else peonHarvest
