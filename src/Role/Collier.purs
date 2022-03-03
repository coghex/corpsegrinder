module Role.Collier where
import UPrelude
import Screeps.Data
import CG

-- | a collier moves to a harvest spot and stays there harvesting until death
preformCollier ∷ Creep → CG Env Unit
preformCollier creep = pure unit
