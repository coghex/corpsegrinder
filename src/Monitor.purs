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
import Spawn
import CorpseGrinder

-- | monitors game situation for various periodic changes and
--   sets memory variables accordingly
monitorCreeps ∷ Spwn Unit
monitorCreeps = do
  s1 ← asks (_.spawn)
  let containers       = Room.find' (RO.room s1) find_structures
                           $ structIsType structure_container
      containerMemObjs = map makeContainerMem containers
  containersMem ← getMemField' "containers"
  case containersMem of
    Nothing → if (length containerMemObjs) > 0 then
                setMemField' "containers" $ containerMemObjs
              else pure unit
    Just cm → setNewContainerMemory cm containerMemObjs

makeContainerMem ∷ Container → ContainerMemory
makeContainerMem container = ContainerMemory { id: id, used: false }
  where id = Structure.id container

setNewContainerMemory ∷ Array ContainerMemory → Array ContainerMemory → Spwn Unit
setNewContainerMemory cm0 cmNew =
  if (length newMem) > (length cm0) then
    setMemField' "containers" newMem
  else pure unit
  where newMem = union cm0 cmNew
