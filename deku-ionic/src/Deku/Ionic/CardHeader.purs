module Deku.Ionic.CardHeader where

import Prelude hiding (max, min)

import Control.Plus (empty)
import Data.Maybe (Maybe(..))
import Deku.Attribute (Attribute)
import Deku.Core (Nut, attributeAtYourOwnRisk, elementify)
import Deku.DOM (HTMLElement)
import Deku.DOM.Self (class IsSelf)
import Deku.Ionic.Enums as E
import FRP.Poll (Poll)
import Type.Proxy (Proxy)

data IonCardHeader

type HTMLIonCardHeader (r :: Row Type) =
  ( __tag :: Proxy "HTMLIonCardHeader"
  , color :: E.Color
  , mode :: E.Mode
  , translucent :: Boolean
  | HTMLElement r
  )

instance IsSelf IonCardHeader "HTMLIonCardHeader"

ionCardHeader
  :: Array (Poll (Attribute (HTMLIonCardHeader ()))) -> Array Nut -> Nut
ionCardHeader = elementify Nothing "ion-card-header"

ionCardHeader_
  :: Array Nut -> Nut
ionCardHeader_ = ionCardHeader empty

-- Attribute functions for Color, Mode, and Translucent
color
  :: forall r. Poll E.Color -> Poll (Attribute (color :: E.Color | r))
color = map (E.unColor >>> attributeAtYourOwnRisk "color")

color_
  :: forall r. E.Color -> Poll (Attribute (color :: E.Color | r))
color_ = pure >>> color

mode
  :: forall r. Poll E.Mode -> Poll (Attribute (mode :: E.Mode | r))
mode = map (E.unMode >>> attributeAtYourOwnRisk "mode")

mode_
  :: forall r. E.Mode -> Poll (Attribute (mode :: E.Mode | r))
mode_ = pure >>> mode

translucent
  :: forall r. Poll Boolean -> Poll (Attribute (translucent :: Boolean | r))
translucent = map (show >>> attributeAtYourOwnRisk "translucent")

translucent_
  :: forall r. Boolean -> Poll (Attribute (translucent :: Boolean | r))
translucent_ = pure >>> translucent
