module Screeps.Flag where

import UPrelude
import Effect (Effect)
import Screeps.Data
import Screeps.FFI

color ∷ Flag → Color
color = unsafeField "color"

memory ∷ ∀ α. Flag → α
memory = unsafeField "memory"

name ∷ Flag → String
name = unsafeField "name"

secondaryColor ∷ Flag → Color
secondaryColor = unsafeField "secondaryColor"

remove ∷ Flag → Effect ReturnCode
remove = runThisEffFn0 "remove"

setColor ∷ Flag → Color → Effect ReturnCode
setColor = runThisEffFn1 "setColor"

setColor' ∷ Flag → Color → Color → Effect ReturnCode
setColor' = runThisEffFn2 "setColor"

setPosition ∷ ∀ α. Flag → TargetPosition α → Effect ReturnCode
setPosition flag (TargetPt  x y) = runThisEffFn2 "setPosition" flag x y
setPosition flag (TargetObj obj) = runThisEffFn1 "setPosition" flag obj
setPosition flag (TargetPos pos) = runThisEffFn1 "setPosition" flag pos
