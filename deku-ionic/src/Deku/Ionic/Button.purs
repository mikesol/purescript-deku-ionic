module Deku.Ionic.Button where

import Prelude hiding (max, min)

import Web.Event.Internal.Types as Web.Event.Internal.Types
import Control.Plus (empty)
import Data.Maybe (Maybe(..))
import Deku.Attribute (Attribute)
import Deku.Core (Nut, attributeAtYourOwnRisk, elementify, callbackWithCaution)
import Deku.DOM (HTMLElement)
import Deku.DOM.Self (class IsSelf)
import Deku.Ionic.Enums as E
import Effect (Effect)
import FRP.Poll (Poll)
import Safe.Coerce (coerce)
import Type.Proxy (Proxy)

data IonButton

type HTMLIonButton (r :: Row Type) =
  ( __tag :: Proxy "HTMLIonButton"
  , slot :: String
  , shape :: String
  , size :: String
  , color :: E.Color
  , disabled :: Boolean
  , download :: String
  , href :: String
  , expand :: E.ButtonExpand
  | HTMLElement r
  )

instance IsSelf IonButton "HTMLIonButton"

ionButton
  :: Array (Poll (Attribute (HTMLIonButton ()))) -> Array Nut -> Nut
ionButton = elementify Nothing "ion-button"

ionButton_
  :: Array Nut -> Nut
ionButton_ = ionButton empty

-- Attribute functions for the properties
slot
  :: forall r. Poll String -> Poll (Attribute (slot :: String | r))
slot = map (attributeAtYourOwnRisk "slot")

slot_
  :: forall r. String -> Poll (Attribute (slot :: String | r))
slot_ = pure >>> slot

shape
  :: forall r. Poll String -> Poll (Attribute (shape :: String | r))
shape = map (attributeAtYourOwnRisk "shape")

shape_
  :: forall r. String -> Poll (Attribute (shape :: String | r))
shape_ = pure >>> shape

size
  :: forall r. Poll String -> Poll (Attribute (size :: String | r))
size = map (attributeAtYourOwnRisk "size")

size_
  :: forall r. String -> Poll (Attribute (size :: String | r))
size_ = pure >>> size

color
  :: forall r. Poll E.Color -> Poll (Attribute (color :: E.Color | r))
color = map (E.unColor >>> attributeAtYourOwnRisk "color")

color_
  :: forall r. E.Color -> Poll (Attribute (color :: E.Color | r))
color_ = pure >>> color

disabled
  :: forall r. Poll Boolean -> Poll (Attribute (disabled :: Boolean | r))
disabled = map (show >>> attributeAtYourOwnRisk "disabled")

disabled_
  :: forall r. Boolean -> Poll (Attribute (disabled :: Boolean | r))
disabled_ = pure >>> disabled

download
  :: forall r. Poll String -> Poll (Attribute (download :: String | r))
download = map (attributeAtYourOwnRisk "download")

download_
  :: forall r. String -> Poll (Attribute (download :: String | r))
download_ = pure >>> download

href
  :: forall r. Poll String -> Poll (Attribute (href :: String | r))
href = map (attributeAtYourOwnRisk "href")

href_
  :: forall r. String -> Poll (Attribute (href :: String | r))
href_ = pure >>> href

expand
  :: forall r. Poll E.ButtonExpand -> Poll (Attribute (expand :: E.ButtonExpand | r))
expand = map (E.unButtonExpand >>> attributeAtYourOwnRisk "expand")

expand_
  :: forall r. E.ButtonExpand -> Poll (Attribute (expand :: E.ButtonExpand | r))
expand_ = pure >>> expand

-- Newtype wrappers for event types
newtype IonBlur = IonBlur Web.Event.Internal.Types.Event
newtype IonFocus = IonFocus Web.Event.Internal.Types.Event

-- Callback functions for focus and blur events using callbackWithCaution
onIonBlur
  :: forall r. Poll (IonBlur -> Effect Unit) -> Poll (Attribute (onIonBlur :: IonBlur | r))
onIonBlur = map ((coerce :: _ -> _ -> _ Unit) >>> map (_ $> true) >>> callbackWithCaution "ionBlur")

onIonFocus
  :: forall r. Poll (IonFocus -> Effect Unit) -> Poll (Attribute (onIonFocus :: IonFocus | r))
onIonFocus = map ((coerce :: _ -> _ -> _ Unit) >>> map (_ $> true) >>> callbackWithCaution "ionFocus")