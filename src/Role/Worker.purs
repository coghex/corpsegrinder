module Role.Worker where

import UPrelude
import Screeps.Data
import Job.Repair (preformJobRepair)
import Data
import CG

preformWorker ∷ Creep → Job → CG Env Unit
preformWorker creep (JobRepair structId) = preformJobRepair creep $ Id structId
preformWorker creep JobNULL              = pure unit
