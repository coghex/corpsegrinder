module Role.Builder where
import UPrelude
import Control.Monad.Trans.Class ( lift )
import Data.Array ( length )
import Screeps.Const ( err_not_in_range, find_construction_sites )
import Screeps.Room       as Room
import Screeps.RoomObject as RO
import Screeps.Data
import Util ( findNearest )
import Data
import Creep.Peon
import Creep.Util ( creepFull, creepEmpty, creepHasEnergy
                  , findAndSetDestAndTarget, creepPos )
import Creep
import CorpseGrinder ( getObjectById' )

-- | a builder moves between mining for energy and building construction sites
preformBuilder ∷ Crp Unit
preformBuilder = do
  building' ← getCreepMemField "building"
  case building' of
    Nothing       → do
      setCreepMemField' "building" false
      preformBuilderF false
    Just building → preformBuilderF building
preformBuilderF ∷ Boolean → Crp Unit
preformBuilderF building = do
  cFull ← creepFull
  if building then do
    cEmpty ← creepEmpty
    cHasEnergy ← creepHasEnergy
    if cEmpty then do
      setCreepMemField' "building" false
    else if cHasEnergy then do
      home' ← getCreepMemField "home"
      case home' of
        Nothing     → log''' LogError "no home in creep memory"
        Just homeid → do
          home'' ← lift $ lift $ getObjectById' homeid
          case home'' of
            Nothing   → log''' LogError "home refers to undefined"
            Just home → do
              let targets = Room.find (RO.room home) find_construction_sites
              cPos ← creepPos
              case (findNearest targets cPos) of
                Nothing → pure unit
                Just nearestTarget → if length targets > 0 then do
                  res ← creepBuild' nearestTarget
                  if (res ≡ err_not_in_range) then do
                    ret ← moveCreepTo' (TargetObj nearestTarget)
                    pure unit
                  else pure unit
                else pure unit
    else pure unit
  else if cFull then setCreepMemField' "building" true
  else getEnergy
