module Role.Worker where

import UPrelude
import Screeps.Data
import Job.Repair (preformJobRepair)
import Data
import CG

preformWorker ∷ Creep → Job → CG Env Unit
preformWorker creep (JobRepair structId) = preformJobRepair creep $ Id structId
-- Job will be set to JobNULL when complete, we go idle here
-- note that we will have to manally unset memory, since we
-- are outside of the standard processor (changing roles is always fine)
preformWorker creep JobNULL              = do
  setCreepMem creep "role" RoleIdle
