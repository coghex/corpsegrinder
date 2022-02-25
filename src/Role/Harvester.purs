module Role.Harvester where
import UPrelude
import Screeps.Data
import Creep.Peon (getEnergy, storeEnergy, freeCapacity)
import CG

-- | a harvester moves between energy source and extension, spawn, or tower
--   the simplest role
preformHarvester ∷ Creep → CG Env Unit
preformHarvester creep = if freeCapacity creep then getEnergy creep else storeEnergy creep
