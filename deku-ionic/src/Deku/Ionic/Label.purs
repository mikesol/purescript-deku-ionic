module Deku.Ionic.Label where

import Prelude

import Control.Plus (empty)
import Data.Maybe (Maybe(..))
import Deku.Attribute (Attribute)
import Deku.Core (Nut, elementify, attributeAtYourOwnRisk)
import Deku.DOM (HTMLElement)
import Deku.DOM.Self (class IsSelf)
import FRP.Poll (Poll)
import Type.Proxy (Proxy)
import Deku.Ionic.Enums as E

data IonLabel

type HTMLIonLabel (r :: Row Type) =
  ( __tag :: Proxy "HTMLIonLabel"
  , color :: E.Color
  , mode :: E.Mode
  , position :: E.LabelPosition
  | HTMLElement r
  )

instance IsSelf IonLabel "HTMLIonLabel"

ionLabel
  :: Array (Poll (Attribute (HTMLIonLabel ()))) -> Array Nut -> Nut
ionLabel = elementify Nothing "ion-label"

ionLabel_
  :: Array Nut
  -> Nut
ionLabel_ = ionLabel empty

color
  :: forall r
   . Poll E.Color
  -> Poll (Attribute (color :: E.Color | r))
color = map (E.unColor >>> attributeAtYourOwnRisk "color")

color_
  :: forall r
   . E.Color
  -> Poll (Attribute (color :: E.Color | r))
color_ = pure >>> color

mode
  :: forall r
   . Poll E.Mode
  -> Poll (Attribute (mode :: E.Mode | r))
mode = map (E.unMode >>> attributeAtYourOwnRisk "mode")

mode_
  :: forall r
   . E.Mode
  -> Poll (Attribute (mode :: E.Mode | r))
mode_ = pure >>> mode

position
  :: forall r
   . Poll E.LabelPosition
  -> Poll (Attribute (position :: E.LabelPosition | r))
position = map (E.unLabelPosition >>> attributeAtYourOwnRisk "position")

position_
  :: forall r
   . E.LabelPosition
  -> Poll (Attribute (position :: E.LabelPosition | r))
position_ = pure >>> position
