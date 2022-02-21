module Role.Harvester where
import UPrelude
import Effect (Effect)
import Effect.Console (log)
import Data.Array (index, length, foldr, filter, zip, head)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Either (Either(..))
import Data.Argonaut.Decode (JsonDecodeError)
import Screeps.Const ( err_not_in_range, find_sources
                     , find_structures, find_my_spawns
                     , resource_energy
                     , structure_spawn, structure_tower
                     , structure_extension )
import Screeps.Memory as Memory
import Screeps.Data
import Screeps.Creep as Creep
import Screeps.Game as Game
import Screeps.Room ( find, find' )
import Screeps.RoomObject as RO
import Screeps.RoomPosition as RP
import Screeps.Store as Store
import Screeps.Source as Source
import Screeps.Structure ( structureType )
import Screeps.Structure.Spawn as Spawn
import Util (distance)
import Data (HarvestSpot(..))

-- | a harvester moves between energy source and extension, spawn, or tower
preformHarvester ∷ Creep → Effect Unit
preformHarvester creep = do
  let freeCapacity = case (RO.storeMaybe creep) of
                       Nothing → 0
                       Just s0 → Store.getFreeCapacity (Creep.store creep)
  if freeCapacity > 0 then do
    let sources = find (RO.room creep) find_sources
        -- room memory is associated with each spawn right now
        spawns  = find (RO.room creep) find_my_spawns
        -- assume one spawn per room
        spawn   = head spawns
    harvSs ← case spawn of
               Nothing → pure []
               Just s1 → do
                 ret ← Spawn.getMemory s1 "harvestSpots"
                 case ret of
                   Left  _  → pure []
                   Right h0 → pure h0
    case (findNearestOpenSource harvSs sources (RO.pos creep)) of
      Nothing → pure unit
      Just nearestSource → do
        harv ← Creep.harvest creep nearestSource
        dest ← Creep.getMemory creep "target"
        case dest of
          Left  _  → do
                     Creep.setMemory creep "target" (Source.id nearestSource)
                     case spawn of
                       Nothing → pure unit
                       Just s0 → setNHarvs harvSs (Source.id nearestSource) s0
          Right d0 → if (d0 ≡ (Source.id nearestSource)) then pure unit
                     else do
                       Creep.setMemory creep "target" (Source.id nearestSource)
                       case spawn of
                         Nothing → pure unit
                         Just s0 → do
                           setNHarvs harvSs (Source.id nearestSource) s0
                           removeNHarvs harvSs d0 s0

        if harv == err_not_in_range then do
          ret ← Creep.moveTo creep (TargetObj nearestSource)
          pure unit
    else pure unit
  else do
    let targets = find' (RO.room creep) find_structures hasFreeSpace
--    log $ "num targets: " <> (show (length targets))
    case (findNearest targets (RO.pos creep)) of
      Nothing → pure unit
      Just nearestTarget → if (length targets) > 0 then do
          targ ← Creep.transfer creep nearestTarget resource_energy
          if targ == err_not_in_range then do
            ret ← Creep.moveTo creep (TargetObj nearestTarget)
            pure unit
          else pure unit
        else pure unit

setNHarvs ∷ Array HarvestSpot → Id Source → Spawn → Effect Unit
setNHarvs harvs sourceId spawn = do
  let newHarvs = map (setNHarvsF sourceId) harvs
  Spawn.setMemory spawn "harvestSpots" newHarvs
setNHarvsF ∷ Id Source → HarvestSpot → HarvestSpot
setNHarvsF sourceId (HarvestSpot { sourceName, nHarvs, nMaxHarvs, harvSpots }) =
  HarvestSpot { sourceName: sourceName, nHarvs:    (nHarvs + n)
              , nMaxHarvs:  nMaxHarvs,  harvSpots: harvSpots }
  where n = if (sourceId ≡ sourceName) then 1 else 0
removeNHarvs ∷ Array HarvestSpot → Id Source → Spawn → Effect Unit
removeNHarvs harvs sourceId spawn = do
  let newHarvs = map (removeNHarvsF sourceId) harvs
  Spawn.setMemory spawn "harvestSpots" newHarvs
removeNHarvsF ∷ Id Source → HarvestSpot → HarvestSpot
removeNHarvsF sourceId (HarvestSpot { sourceName, nHarvs, nMaxHarvs, harvSpots }) =
  HarvestSpot { sourceName: sourceName, nHarvs:    (nHarvs - n)
              , nMaxHarvs:  nMaxHarvs,  harvSpots: harvSpots }
  where n = if (sourceId ≡ sourceName) then 1 else 0

hasFreeSpace ∷ ∀ a. Structure a → Boolean
hasFreeSpace structure
  =  (isStorable structure (structureType structure))
  && (spawnStoreCapacity > 0)
  where spawnStoreCapacity = case (RO.storeMaybe structure) of
                               Nothing → 0
                               Just s0 → Store.getFreeCapacity' s0 resource_energy
isStorable ∷ ∀ a. Structure a → StructureType → Boolean
isStorable structure structure_spawn     = true
isStorable structure structure_tower     = true
isStorable structure structure_extension = true
isStorable _         _                   = false

findNearest ∷ ∀ α. Array (RoomObject α) → RoomPosition → Maybe (RoomObject α)
findNearest arr pos0 = index arr ind
  where ind    = foldr min 0 dists
        dists  = map (findDistance x0 y0) arr
        x0     = RP.x   pos0
        y0     = RP.y   pos0
findDistance ∷ ∀ α. Int → Int → RoomObject α → Int
findDistance x0 y0 obj = distance x1 y1 x0 y0
  where pos1 = RO.pos obj
        x1   = RP.x   pos1
        y1   = RP.y   pos1

-- will only return a source if it has capacity
findNearestOpenSource ∷ Array HarvestSpot → Array Source → RoomPosition → Maybe Source
-- if there was an error in reading harvestspots, just throw them away
--findNearestOpenSource [] arr pos0 = findNearest arr pos0
findNearestOpenSource harvData arr pos0 = index arr ind
  where ind    = foldr min 0 dists
        dists  = map (findSourceDistance x0 y0) (zip harvData arr)
        x0     = RP.x pos0
        y0     = RP.y pos0
findSourceDistance ∷ Int → Int → Tuple HarvestSpot Source → Int
findSourceDistance x0 y0 (Tuple spot source) = if avail then distance x0 y0 x1 y1
                                               -- TODO: figure out the optimal value
                                               --       for when we can begin doubling
                                               --       up on creeps
                                               else 10000
  where pos1   = RO.pos source
        x1     = RP.x   pos1
        y1     = RP.y   pos1
        avail  = spotAvailable spot
spotAvailable ∷ HarvestSpot → Boolean
spotAvailable (HarvestSpot { sourceName, nHarvs, nMaxHarvs, harvSpots })
  = nHarvs < nMaxHarvs
