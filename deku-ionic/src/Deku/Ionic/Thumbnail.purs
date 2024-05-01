module Deku.Ionic.Thumbnail where

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

data IonThumbnail

type HTMLIonThumbnail (r :: Row Type) =
  ( __tag :: Proxy "HTMLIonThumbnail"
  , slot :: String
  | HTMLElement r
  )

instance IsSelf IonThumbnail "HTMLIonThumbnail"

ionThumbnail
  :: Array (Poll (Attribute (HTMLIonThumbnail ()))) -> Array Nut -> Nut
ionThumbnail = elementify Nothing "ion-thumbnail"

ionThumbnail_
  :: Array Nut -> Nut
ionThumbnail_ = ionThumbnail empty

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
