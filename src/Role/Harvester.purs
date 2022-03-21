module Role.Harvester where
import UPrelude
import Data ( Role(..) )
import Control.Monad.Trans.Class ( lift )
import Screeps.Room       as Room
import Screeps.RoomObject as RO
import Creep.Peon ( peonMove, peonHarvest, peonDeposit )
import Creep.Util ( findAndSetDestAndTarget, creepEmpty, creepFull )
import Util ( spotToPos )
import Creep
import CorpseGrinder ( getObjectById' )

-- | a harvester is only for low RCL, and mines energy after mining
--   to where its needed.  once we have containers we no longer use
preformHarvester ∷ Crp Unit
preformHarvester = do
  moving' ← getCreepMemField "moving"
  let moving = case moving' of
                 Nothing → false
                 Just b0 → b0
  if moving then do
    dest' ← getCreepMemField "dest"
    case dest' of
      Nothing → do
        dest ← findAndSetDestAndTarget RoleHarvester
        peonMove dest
      Just dest → do
        home' ← getCreepMemField "home"
        case home' of
          Nothing     → pure unit
          Just homeid → do
            home'' ← lift $ lift $ getObjectById' homeid
            case home'' of
              Nothing → pure unit
              Just home → peonMove $ spotToPos room dest
                where room = Room.name (RO.room home)
  else do
    cFull ← creepFull
    harvesting' ← getCreepMemField "harvesting"
    harvesting  ← case harvesting' of
                    Nothing → do
                      let bool = not $ cFull
                      setCreepMemField' "harvesting" bool
                      pure bool
                    Just h0 → pure h0
    if harvesting then
      if cFull then do
        setCreepMemField' "harvesting" false
        setCreepMemField' "moving" true
        dest ← findAndSetDestAndTarget RoleHarvester
        peonMove dest
      else peonHarvest
    else peonDeposit

