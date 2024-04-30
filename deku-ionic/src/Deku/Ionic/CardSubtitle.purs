module Deku.Ionic.CardSubtitle where

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

data IonCardSubtitle

type HTMLIonCardSubtitle (r :: Row Type) =
  ( __tag :: Proxy "HTMLIonCardSubtitle"
  , color :: E.Color
  , mode :: E.Mode
  | HTMLElement r
  )

instance IsSelf IonCardSubtitle "HTMLIonCardSubtitle"

ionCardSubtitle
  :: Array (Poll (Attribute (HTMLIonCardSubtitle ()))) -> Array Nut -> Nut
ionCardSubtitle = elementify Nothing "ion-card-subtitle"

ionCardSubtitle_
  :: Array Nut -> Nut
ionCardSubtitle_ = ionCardSubtitle empty

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
