module Deku.Ionic.Note where

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

data IonNote

type HTMLIonNote (r :: Row Type) =
  ( __tag :: Proxy "HTMLIonNote"
  , slot :: String
  , color :: E.Color
  , mode :: E.Mode
  | HTMLElement r
  )

instance IsSelf IonNote "HTMLIonNote"

ionNote
  :: Array (Poll (Attribute (HTMLIonNote ()))) -> Array Nut -> Nut
ionNote = elementify Nothing "ion-note"

ionNote_
  :: Array Nut -> Nut
ionNote_ = ionNote empty

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
