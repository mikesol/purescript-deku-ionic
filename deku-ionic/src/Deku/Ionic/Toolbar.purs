module Deku.Ionic.Toolbar where

import Prelude

import Control.Plus (empty)
import Data.Maybe (Maybe(..))
import Deku.Attribute (Attribute)
import Deku.Core (Nut, attributeAtYourOwnRisk, elementify)
import Deku.DOM (HTMLElement)
import Deku.Ionic.Enums as E
import FRP.Poll (Poll)
import Type.Proxy (Proxy)

type HTMLIonToolbar (r :: Row Type) =
  ( __tag :: Proxy "HTMLIonToolbar"
  , color :: String
  , mode :: String
  | HTMLElement r
  )

ionToolbar
  :: Array (Poll (Attribute (HTMLIonToolbar ()))) -> Array Nut -> Nut
ionToolbar = elementify Nothing "ion-toolbar"


ionToolbar_
  :: Array Nut
  -> Nut
ionToolbar_ = ionToolbar empty

color
  :: forall r
   . Poll E.Color
  -> Poll (Attribute (color :: String | r))
color = map (E.unColor >>> attributeAtYourOwnRisk "color")

color_
  :: forall r
   . E.Color
  -> Poll (Attribute (color :: String | r))
color_ = pure >>> color


mode
  :: forall r
   . Poll E.Mode
  -> Poll (Attribute (mode :: String | r))
mode = map (E.unMode >>> attributeAtYourOwnRisk "mode")

mode_
  :: forall r
   . E.Mode
  -> Poll (Attribute (mode :: String | r))
mode_ = pure >>> mode
