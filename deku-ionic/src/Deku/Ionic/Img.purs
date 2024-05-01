module Deku.Ionic.Img where

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

data IonImg

type HTMLIonImg (r :: Row Type) =
  ( __tag :: Proxy "HTMLIonImg"
  , slot :: String
  , alt :: String
  , src :: String
  | HTMLElement r
  )

instance IsSelf IonImg "HTMLIonImg"

ionImg
  :: Array (Poll (Attribute (HTMLIonImg ()))) -> Array Nut -> Nut
ionImg = elementify Nothing "ion-thumbnail"

ionImg_
  :: Array Nut -> Nut
ionImg_ = ionImg empty

-- Attribute functions for all properties
slot
  :: forall r. Poll String -> Poll (Attribute (slot :: String | r))
slot = map (attributeAtYourOwnRisk "slot")

slot_
  :: forall r. String -> Poll (Attribute (slot :: String | r))
slot_ = pure >>> slot

alt
  :: forall r. Poll String -> Poll (Attribute (alt :: String | r))
alt = map (attributeAtYourOwnRisk "alt")

alt_
  :: forall r. String -> Poll (Attribute (alt :: String | r))
alt_ = pure >>> alt

src
  :: forall r. Poll String -> Poll (Attribute (src :: String | r))
src = map (attributeAtYourOwnRisk "src")

src_
  :: forall r. String -> Poll (Attribute (src :: String | r))
src_ = pure >>> src

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
