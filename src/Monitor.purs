module Monitor where
import UPrelude
import Data.Array (uncons, head, tail, length, elem, union)
import Control.Monad.Reader (asks)
import Screeps.Data
import Screeps.Game as Game
import Screeps.Room as Room
import Screeps.RoomObject as RO
import Screeps.Structure as Structure
import Screeps.Const (find_structures, structure_container)
import Foreign.Object as F
import Util (structIsType)
import Data
import CG

-- | monitors game situation for various periodic changes and
--   sets memory variables accordingly
monitorCreeps ∷ CG Env Unit
monitorCreeps = do
  game ← asks (_.game)
  let spawn1 = F.lookup "Spawn1" spawnslist
      spawnslist = Game.spawns game
  case spawn1 of
    Nothing → log' LogError "no spawn1"
    Just s1 → do
      let containers       = Room.find' (RO.room s1) find_structures
                               $ structIsType structure_container
          containerMemObjs = map makeContainerMem containers
      containersMem ← getMemField "containers"
      case containersMem of
        Nothing → if (length containerMemObjs) > 0 then
                    setMemField "containers" $ containerMemObjs
                  else pure unit
        Just cm → setNewContainerMemory cm containerMemObjs

makeContainerMem ∷ Container → ContainerMemory
makeContainerMem container = ContainerMemory { id: id, used: false }
  where id = Structure.id container

setNewContainerMemory ∷ Array ContainerMemory → Array ContainerMemory → CG Env Unit
setNewContainerMemory cm0 cmNew =
  if (length newMem) > (length cm0) then
    setMemField "containers" newMem
  else pure unit
  where newMem = union cm0 cmNew
