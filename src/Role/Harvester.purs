module Role.Harvester where
import UPrelude
import Control.Monad.Trans.Class (lift)
import Control.Monad.Reader.Class (asks)
import Screeps.RoomObject as RO
import Screeps.Room as Room
import Screeps.Data
import Creep.Peon (peonHarvest, peonMove, peonDeposit)
import Util (spotToPos)
import Data (Role(..))
import Creep.Util (findAndSetDestAndTarget, creepFull)
import Creep
import CG

-- | a harvester moves between energy source and extension, spawn, or tower
--   the simplest role
preformHarvester ∷ Creep → CE CreepEnv (CG Env) Unit
--preformHarvester creep = if creepFull creep then storeEnergy creep else getEnergy creep
preformHarvester creep = do
  moving ← getCreepMemMoving
  lift $ do
    if moving then do
      dest' ← getCreepMem creep "dest"
      case dest' of
        Nothing → do
          dest ← findAndSetDestAndTarget RoleHarvester creep
          peonMove creep dest
        Just dest → peonMove creep $ spotToPos room dest
          where room = Room.name (RO.room creep)
    else do
      harvesting' ← getCreepMem creep "harvesting"
      harvesting  ← case harvesting' of
                      Nothing → do
                        let bool = not $ creepFull creep
                        setCreepMem creep "harvesting" bool
                        pure bool
                      Just h0 → pure h0
      if harvesting then
        if creepFull creep then do
          setCreepMem creep "harvesting" false
          setCreepMem creep "moving" true
          dest ← findAndSetDestAndTarget RoleHarvester creep
          peonMove creep dest
        else peonHarvest creep
      else peonDeposit creep
