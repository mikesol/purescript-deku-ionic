module Deku.Ionic.ListHeader where

import Prelude 

import Control.Plus (empty)
import Data.Maybe (Maybe(..))
import Deku.Attribute (Attribute)
import Deku.Core (Nut, attributeAtYourOwnRisk, elementify)
import Deku.DOM (HTMLElement)
import Deku.DOM.Self (class IsSelf)
import Deku.Ionic.Enums as E
import FRP.Poll (Poll)
import Type.Proxy (Proxy)

data IonListHeader

type HTMLIonListHeader (r :: Row Type) =
  ( __tag :: Proxy "HTMLIonListHeader"
  , slot :: String
  , color :: E.Color
  , mode :: E.Mode
  , lines :: E.Lines
  | HTMLElement r
  )

instance IsSelf IonListHeader "HTMLIonListHeader"

ionListHeader
  :: Array (Poll (Attribute (HTMLIonListHeader ()))) -> Array Nut -> Nut
ionListHeader = elementify Nothing "ion-list-header"

ionListHeader_
  :: Array Nut -> Nut
ionListHeader_ = ionListHeader empty

-- Attribute functions for all properties
slot
  :: forall r. Poll String -> Poll (Attribute (slot :: String | r))
slot = map (attributeAtYourOwnRisk "slot")

slot_
  :: forall r. String -> Poll (Attribute (slot :: String | r))
slot_ = pure >>> slot

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


lines
  :: forall r. Poll E.Lines
  -> Poll (Attribute (lines :: E.Lines | r))
lines = map (E.unLines >>> attributeAtYourOwnRisk "lines")

lines_
  :: forall r. E.Lines
  -> Poll (Attribute (lines :: E.Lines | r))
lines_ = pure >>> lines
