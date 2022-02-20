module Screeps.Creep where

import UPrelude
import Effect (Effect)
import Data.Either (Either)
import Screeps.Data
import Screeps.FFI
import Data.Argonaut.Decode (class DecodeJson, decodeJson, JsonDecodeError)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)

foreign import unsafeGetAllCreepEff :: ∀ val. String → Effect val
foreign import unsafeGetCreepEff :: ∀ val. String → String → Effect val
foreign import unsafeSetCreepEff :: ∀ val. String → String → val → Effect Unit

body ∷ Creep → Array BodyPart
body = unsafeField "body"

fatigue ∷ Creep → Int
fatigue = unsafeField "fatigue"

hits ∷ Creep → Int
hits = unsafeField "hits"

hitsMax ∷ Creep → Int
hitsMax = unsafeField "hitsMax"

getId ∷ Creep → Id Creep
getId = unsafeField "id"

getIdAsStr ∷ Creep → String
getIdAsStr = unsafeField "id"

--memory ∷ ∀ α. (DecodeJson α) ⇒ Creep → Effect (Either JsonDecodeError α)
--memory = decodeJson <$> unsafeGetAllCreepEff
--
--getMemory ∷ ∀ α. (DecodeJson α) ⇒ Creep → String → Effect (Either JsonDecodeError α)
--getMemory = decodeJson <$> unsafeGetCreepEff
--
--setMemory ∷ ∀ α. (EncodeJson α) ⇒ Creep → String → α → Effect Unit
--setMemory creep key val = decodeJson
--  <$> unsafeSetCreepEff creep key $ encodeJson val

my ∷ Creep → Boolean
my = unsafeField "my"

name ∷ Creep → String
name = unsafeField "name"

owner ∷ Creep → { username ∷ String }
owner = unsafeField "owner"

saying ∷ Creep → String
saying = unsafeField "saying"

spawning ∷ Creep → Boolean
spawning = unsafeField "spawning"

store ∷ Creep → Store
store = unsafeField "store"

ticksToLive ∷ Creep → Int
ticksToLive = unsafeField "ticksToLive"

attack ∷ ∀ α. Creep → RoomObject α → Effect ReturnCode
attack = runThisEffFn1 "attack"

attackController ∷ Creep → Controller → Effect ReturnCode
attackController = runThisEffFn1 "attackController"

build ∷ Creep → ConstructionSite → Effect ReturnCode
build = runThisEffFn1 "build"

cancelOrder ∷ Creep → String → Effect ReturnCode
cancelOrder = runThisEffFn1 "cancelOrder"

claimController ∷ Creep → Controller → Effect ReturnCode
claimController = runThisEffFn1 "claimController"

dismantle ∷ ∀ α. Creep → Structure α → Effect ReturnCode
dismantle = runThisEffFn1 "dismantle"

drop ∷ Creep → ResourceType → Effect ReturnCode
drop = runThisEffFn1 "drop"

generateSafeMode ∷ Creep → Controller → Effect ReturnCode
generateSafeMode = runThisEffFn1 "generateSafeMode"

getActiveBodyParts ∷ Creep → BodyPartType → Effect Int
getActiveBodyParts = runThisEffFn1 "getActiveBodyParts"

harvest ∷ ∀ α. Creep → RoomObject α → Effect ReturnCode
harvest = runThisEffFn1 "harvest"

heal ∷ Creep → Creep → Effect ReturnCode
heal = runThisEffFn1 "heal"

move ∷ Creep → Direction → Effect ReturnCode
move = runThisEffFn1 "move"

moveByPath ∷ Creep → Path → Effect ReturnCode
moveByPath = runThisEffFn1 "moveByPath"

moveTo ∷ ∀ α. Creep → TargetPosition α → Effect ReturnCode
moveTo creep (TargetPt  x y) = runThisEffFn2 "moveTo" creep x y
moveTo creep (TargetPos pos) = runThisEffFn1 "moveTo" creep pos
moveTo creep (TargetObj obj) = runThisEffFn1 "moveTo" creep obj

notifyWhenAttacked ∷ Creep → Boolean → Effect ReturnCode
notifyWhenAttacked = runThisEffFn1 "notifyWhenAttacked"

pickup ∷ Creep → Resource → Effect ReturnCode
pickup = runThisEffFn1 "pickup"

pull ∷ Creep → Creep → Effect ReturnCode
pull = runThisEffFn1 "pull"

rangedAttack ∷ ∀ α. Creep → RoomObject α → Effect ReturnCode
rangedAttack = runThisEffFn1 "rangedAttack"

rangedHeal ∷ ∀ α. Creep → RoomObject α → Effect ReturnCode
rangedHeal = runThisEffFn1 "rangedHeal"

rangedMassAttack ∷ ∀ α. Creep → Effect ReturnCode
rangedMassAttack = runThisEffFn0 "rangedMassAttack"

repair ∷ ∀ α. Creep → Structure α → Effect ReturnCode
repair = runThisEffFn1 "repair"

reserverController ∷ Creep → Controller → Effect ReturnCode
reserverController = runThisEffFn1 "reserverController"

say ∷ Creep → String → Effect ReturnCode
say = runThisEffFn1 "say"

-- sayPublic ∷ Creep → String → Effect ReturnCode
-- sayPublic creep msg = runThisEffFn2 "say" creep msg true
-- 
-- signController ∷ Creep → Controller → String → Effect ReturnCode
-- signController = runThisEffFn2 "signController"
-- 
-- suicide ∷ Creep → Effect ReturnCode
-- suicide = runThisEffFn0 "suicide"
-- 
-- transfer ∷ Creep → RoomObject α → ResourceType → Effect ReturnCode
-- transfer = runThisEffFn2 "transfer"
-- 
-- transfer' ∷ Creep → RoomObject α → ResourceType → Int → Effect ReturnCode
-- transfer' = runThisEffFn3 "transfer"
-- 
-- upgradeController ∷ Creep → Controller → Effect ReturnCode
-- upgradeController = runThisEffFn1 "upgradeController"
-- 
-- withdraw ∷ Creep → RoomObject → ResourceType → Effect ReturnCode
-- withdraw = runThisEffFn2 "withdraw"
-- 
-- withdraw' ∷ Creep → RoomObject → ResourceType → Int → Effect ReturnCode
-- withdraw' = runThisEffFn3 "withdraw"
