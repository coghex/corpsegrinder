module Role.Collier where
import UPrelude
import Control.Monad.Reader (asks)
import Data.Array (head)
import Data.Maybe (Maybe(..))
import Screeps.Data
import Screeps.Room as Room
import Screeps.RoomObject as RO
import Screeps.Structure as Structure
import Screeps.Const (structure_container, find_structures)
import Creep.Collier (collierHarvest, collierMove)
import Util (structIsType, findSpot)
import Data
import CG

-- | a collier moves to a harvest spot and stays there harvesting until death
preformCollier ∷ Creep → CG Env Unit
preformCollier creep = do
  harvesting ← getCreepMem creep "harvesting"
  case harvesting of
    -- this would signify an uninitiated creep
    Nothing → setCreepMem creep "harvesting" false
    Just h0 → if h0 then collierHarvest creep else do
        target' ← getCreepMem creep "target"
        case target' of
          Nothing → do
            -- it doesnt matter the order in which we send these
            containers ← getMemField "containers"
            case containers of
              Nothing → pure unit
              Just cs → case head cs of
                Nothing → pure unit
                Just c0 → do
                  container0 ← getContainerByMemory c0
                  case container0 of
                    Nothing → log' LogWarn "container no longer exists"
                    Just containerObj → preformCollierF creep containerObj
          Just id0 → do
            obj ← getObjectById' id0
            case obj of
              Nothing → do
                containers ← getMemField "containers"
                case containers of
                  Nothing → pure unit
                  Just cs → case head cs of
                    Nothing → pure unit
                    Just c0 → do
                      container0 ← getContainerByMemory c0
                      case container0 of
                        Nothing → log' LogWarn "container no longer exists"
                        Just containerObj → preformCollierF creep containerObj
              Just obj0 → preformCollierF creep obj0

preformCollierF ∷ Creep → Container → CG Env Unit
preformCollierF creep containerObj = do
  pathMem ← getCreepMem creep "path"
  path0 ← case pathMem of
    Nothing → do
      let path = Room.findPath (RO.room creep) (RO.pos creep) (RO.pos containerObj)
      setCreepMem creep "path" path
      setCreepMem creep "dest" $ findSpot containerObj
      setCreepMem creep "target" $ Structure.id containerObj
      pure path
    Just [] → do
      let path = Room.findPath (RO.room creep) (RO.pos creep) (RO.pos containerObj)
      setCreepMem creep "path" path
      setCreepMem creep "dest" $ findSpot containerObj
      setCreepMem creep "target" $ Structure.id containerObj
      pure path
    Just p0 → pure p0
  collierMove creep path0
