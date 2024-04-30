module Deku.Ionic.CardTitle where

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

data IonCardTitle

type HTMLIonCardTitle (r :: Row Type) =
  ( __tag :: Proxy "HTMLIonCardTitle"
  , color :: E.Color
  , mode :: E.Mode
  | HTMLElement r
  )

instance IsSelf IonCardTitle "HTMLIonCardTitle"

ionCardTitle
  :: Array (Poll (Attribute (HTMLIonCardTitle ()))) -> Array Nut -> Nut
ionCardTitle = elementify Nothing "ion-card-title"

ionCardTitle_
  :: Array Nut -> Nut
ionCardTitle_ = ionCardTitle empty

-- Attribute functions for Color and Mode
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
