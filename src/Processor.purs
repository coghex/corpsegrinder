module Processor where

import UPrelude
import Control.Monad.Trans.Class ( lift )
import Control.Monad.Reader ( asks )
import Data.Argonaut.Core ( Json )
import Data.Argonaut.Encode ( encodeJson )
import Data.Argonaut.Decode ( decodeJson )
import Data.Array ( index, uncons )
import Data.Int ( quot, round, toNumber )
import Foreign.Object as F
import Screeps.Creep  as Creep
import Screeps.Game   as Game
import Screeps.Data
import Spawn
import Creep
import Data

processCreeps ∷ Spwn Unit
processCreeps = do
  time       ← lift $ asks (_.time)
  creepsMem' ← getMemField' "creeps"
  creeps     ← getCreeps'
  let utl0      = F.fold addUpUtils 0 creepsMem
      creepsMem = case creepsMem' of
                    Nothing → F.empty
                    Just c0 → c0
      creepSkip = time `mod` (F.size creepsMem)
      creep0    = case (F.keys creepsMem) `index` creepSkip of
                    Nothing → "noname"
                    Just n0 → n0
  -- sets the global utility every time
  setMemField' "utility" utl0
  -- creep processed in own monadic reader structure
  case (F.lookup creep0 creepsMem) of
    Nothing → if (F.size creepsMem) > 1 then 
                log'' LogError "creep array and memory dont match"
              else pure unit
    Just m0 → do
      case (F.lookup creep0 creeps) of
        Nothing → log'' LogError "creep has memory but no object"
        Just c0 → runCE processCreep { creep:c0, mem:m0 }

-- | decide on role and/or job
processCreep ∷ Crp Unit
processCreep = do
  creep0Obj ← asks (_.creep)
  creeps' ← lift $ getMemField' "creeps"
  isBusy  ← findIfBusy
  if isBusy then pure unit else do
    game     ← lift $ lift $ asks (_.game)
    creepU0' ← getCreepMemField "utility"
    creepPos ← getCreepPos
    type0'   ← getCreepMemField "typ"
    role0'   ← getCreepMemField "role"
    jobs     ← lift getAvailRepairJobs
    let newUtl   = fromMaybeToMaybeJson $ scores `index` ind
        creep0   = Creep.name creep0Obj
        ind      = findInd roleList alt 0
        newRole  = Just $ encodeJson alt
        alt      = bestRole scores roleList 0 RoleNULL
        scores   = map (calcRoleScore creepU0 creepPos energyNeed
                                      jobs creeps type0 role0) roleList
        type0    = case type0' of
                     Nothing → CreepNULL
                     Just t0 → t0
        role0    = case role0' of
                     Nothing → RoleNULL
                     Just r0 → r0
        creepU0  = case creepU0' of
                     Nothing → 0
                     Just u0 → u0
        creeps   = case creeps' of
                     Nothing → F.empty
                     Just c0 → c0
        -- plus one is solely to avoid hopefully rare case where capacity is 0
        energyNeed  = round $ 100.0 * energyNeed'
        energyNeed' = (toNumber (energyFreeSpace spawns))
                    / (toNumber $ (energyCapacity spawns) + 1)
        spawns      = Game.spawns game
    newCreep ← switchRole alt
    setCreepMem' newCreep
    setCreepMemField' "utility" newUtl
    setCreepMemField' "role"    newRole

-- | we need to always return something or foreign.update will delete things
fromMaybeToMaybeJson ∷ Maybe Int → Maybe Json
fromMaybeToMaybeJson (Nothing) = Just $ encodeJson (-1)
fromMaybeToMaybeJson (Just v)  = Just $ encodeJson (v)

findIfBusy ∷ Crp Boolean
findIfBusy = do
  upgrading ← getCreepMemField "upgrading"
  building  ← getCreepMemField "building"
  repairing ← getCreepMemField "repairing"
  pure $ findIfBusyF upgrading building repairing
findIfBusyF ∷ Maybe Boolean → Maybe Boolean → Maybe Boolean → Boolean
findIfBusyF Nothing  Nothing  Nothing  = false
findIfBusyF (Just v) Nothing  Nothing  = v
findIfBusyF Nothing  (Just v) Nothing  = v
findIfBusyF Nothing  Nothing  (Just v) = v
-- if we get to this last case we fucked up
findIfBusyF _        _        _        = false

-- | used in a Foreign.fold() to add up utility field of json objects
addUpUtils ∷ Int → String → F.Object Json → Int
addUpUtils n key val = case (F.lookup "utility" val) of
  Nothing → n
  Just v0 → case (decodeJson v0) of
    Left  _  → n
    Right n0 → n + n0

-- | TODO: be a good programmer and make this Array a → a → Int → Int
findInd ∷ Array Role → Role → Int → Int
findInd []    _   n = n
findInd array val n =
  if (val == val') then n else findInd array' val (n + 1)
  where val'   = case uncons array of
                   Just {head: a, tail: _}  → a
                   Nothing                  → RoleNULL
        array' = case uncons array of
                   Just {head: _, tail: al} → al
                   Nothing                  → []

-- | helps translate a creep to a new role
--   , usually will fail so it doesnt error
switchRole ∷ Role → Crp (F.Object Json)
switchRole newrole = do
  creep0 ← getCreepMem''
  case F.lookup "role" creep0 of
    Nothing → pure creep0
    Just r0 → case decodeJson r0 of
                  Left  _  → pure creep0
                  Right r1 → switchRoleF creep0 r1 newrole
switchRoleF ∷ F.Object Json → Role → Role → Crp (F.Object Json)
switchRoleF creep0 role0 newrole = case role0 of
  RoleUpgrader → case newrole of
  -- TODO: these wont work for more than just the repair job
    (RoleWorker _)  → do
      v' ← getCreepMemField "upgrading"
      let v = case v' of
                Nothing → false
                Just v0 → v0
          creep1 = F.delete "upgrading" creep0
          creep2 = F.update (\_ → Just $ encodeJson v)
                            "repairing" creep1
      pure creep2
    RoleBuilder → do
      v' ← getCreepMemField "upgrading"
      let v      = case v' of
                     Nothing → false
                     Just v0 → v0
          creep1 = F.delete "upgrading" creep0
          creep2 = F.update (\_ → Just $ encodeJson v)
                            "building" creep1
      pure creep2 
    _               → pure creep0
  RoleBuilder → case newrole of
    (RoleWorker _)  → do
      v' ← getCreepMemField "building"
      let v      = case v' of
                     Nothing → false
                     Just v0 → v0
          creep1 = F.delete "building" creep0
          creep2 = F.update (\_ → Just $ encodeJson v)
                            "repairing" creep1
      pure creep2
    RoleUpgrader    → do
      v' ← getCreepMemField "building"
      let v      = case v' of
                     Nothing → false
                     Just v0 → v0
          creep1 = F.delete "building"   creep0
          creep2 = F.update (\_ → Just $ encodeJson v)
                            "upgrading" creep1
      pure creep2
    _               → pure creep0
  _            → pure creep0


-- | returns the best possible role given game state
bestRole ∷ Array Int → Array Role → Int → Role → Role
bestRole []     _     _     role = role
bestRole _      []    _     role = role
bestRole scores roles score role = if (score' > score) then bestRole scores' roles' score' role'
   else bestRole scores' roles' score role
   where roles'  = case uncons roles of
                     Just {head: _, tail: rs} → rs
                     Nothing → []
         role'   = case uncons roles of
                     Just {head: r, tail: _}  → r
                     Nothing → RoleNULL
         scores' = case uncons scores of
                     Just {head: _, tail: ss} → ss
                     Nothing → []
         score'  = case uncons scores of
                     Just {head: s, tail: _}  → s
                     Nothing → 0

-- | finds the score for a given role for a given creep, averages with current score
--   first checks creep type as different creep types value roles differently
calcRoleScore ∷ ∀ α. Int → TargetPosition α → Int → Array Job → F.Object (F.Object Json) → CreepType → Role → Role → Int
calcRoleScore _    _   _          _    _      CreepPeon _     RoleNULL        = 0
calcRoleScore _    _   _          _    _      CreepPeon _     RoleIdle        = 1
calcRoleScore utl0 pos energyNeed jobs creeps CreepPeon role0 RoleUpgrader    = avgScores utl0 score
  where score     = 1000 `quot` ((3*upgraders) + 1)
        upgraders = iDoThat + numberOfRole RoleUpgrader creeps
        iDoThat   = case role0 of
                      RoleUpgrader → -1
                      _            → 0
calcRoleScore utl0 pos energyNeed jobs creeps CreepPeon role0 RoleBuilder = avgScores utl0 score
  where score    = 1000 `quot` (builders + 1)
        builders = iDoThat + numberOfRole RoleBuilder creeps
        iDoThat  = case role0 of
                     RoleBuilder → -1
                     _           → 0
calcRoleScore utl0 pos energyNeed jobs creeps CreepPeon role0 (RoleWorker _)  = avgScores utl0 score
  where score     = calcJobValue jobvalue
        jobvalue  = calcBestJob pos jobs
calcRoleScore utl0 pos energyNeed jobs creeps CreepPeon role0 RoleHarvester   = avgScores utl0 score
-- we wont ever really need more than a couple
  where score   = (100*energyNeed) `quot` ((2*harvs) + 1)

        harvs   = iDoThat + numberOfRole RoleHarvester creeps
        iDoThat = case role0 of
                    RoleHarvester → -1
                    _             → 0
calcRoleScore utl0 pos energyNeed jobs creeps CreepCollier role0 RoleCollier  = 1000
calcRoleScore _    _   _          _    _      _         _     _               = 0

-- | job value is based on distance and type of job
calcBestJob ∷ ∀ α. TargetPosition α → Array Job → Maybe Job
calcBestJob _   []   = Nothing
calcBestJob pos jobs = jobs `index` calcBestJobF 0 pos jobs 0 0
calcBestJobF ∷ ∀ α. Int → TargetPosition α → Array Job → Int → Int → Int
calcBestJobF _ _   []  _     index = index
calcBestJobF n pos arr score index = calcBestJobF n' pos arr' score' index'
  where arr' = case uncons arr of
                 Nothing              → []
                 Just {head:_,tail:a} → a
        job  = case uncons arr of
                 Nothing              → Nothing
                 Just {head:j,tail:_} → Just j
        jobScore = calcJobValue job
        score' = if jobScore > score then jobScore else score
        index' = if jobScore > score then n else index
        n'     = n + 1

-- | TODO: figure out how to use position here
calcJobValue ∷ Maybe Job → Int
calcJobValue Nothing               = 0
calcJobValue (Just JobNULL)        = 0
calcJobValue (Just (JobRepair id)) = 100

-- | weighted average of old utility and new utility
avgScores ∷ Int → Int → Int
avgScores a b = (a + b) `quot` 2

numberOfRole ∷ Role → F.Object (F.Object Json) → Int
numberOfRole role creeps = F.size $ F.filter (roleFilter role) creeps

roleFilter ∷ Role → F.Object Json → Boolean
roleFilter role creep = case (F.lookup "role" creep) of
  Nothing → false
  Just r0 → case decodeJson r0 of
    Left  _  → false
    Right r0 → r0 ≡ role

