module Deku.Ionic.Content where

import Prelude hiding (max, min)

import Control.Plus (empty)
import Control.Promise (Promise)
import Data.Maybe (Maybe(..))
import Deku.Attribute (Attribute)
import Deku.Core (Nut, attributeAtYourOwnRisk, callbackWithCaution, elementify)
import Deku.DOM (HTMLElement)
import Deku.DOM.Self (class IsSelf)
import Deku.Ionic.Enums as E
import Effect (Effect)
import FRP.Poll (Poll)
import Safe.Coerce (coerce)
import Type.Proxy (Proxy)
import Web.Event.Internal.Types as Web.Event.Internal.Types
import Web.HTML as Web.HTML

data IonContent

type HTMLIonContent (r :: Row Type) =
  ( __tag :: Proxy "HTMLIonContent"
  , color :: E.Color
  , forceOverscroll :: Boolean
  , fullscreen :: Boolean
  , scrollEvents :: Boolean
  , scrollX :: Boolean
  , scrollY :: Boolean
  , onIonScroll :: IonScroll
  , onIonScrollStart :: IonScrollStart
  , onIonScrollEnd :: IonScrollEnd
  | HTMLElement r
  )

instance IsSelf IonContent "HTMLIonContent"

ionContent
  :: Array (Poll (Attribute (HTMLIonContent ()))) -> Array Nut -> Nut
ionContent = elementify Nothing "ion-content"

ionContent_
  :: Array Nut -> Nut
ionContent_ = ionContent empty

color
  :: forall r. Poll E.Color -> Poll (Attribute (color :: String | r))
color = map (E.unColor >>> attributeAtYourOwnRisk "color")

color_
  :: forall r. E.Color -> Poll (Attribute (color :: String | r))
color_ = pure >>> color

-- Newtype wrappers for event handling
newtype IonScroll = IonScroll Web.Event.Internal.Types.Event
newtype IonScrollStart = IonScrollStart Web.Event.Internal.Types.Event
newtype IonScrollEnd = IonScrollEnd Web.Event.Internal.Types.Event

-- Event listener direct assignment functions
onIonScroll_
  :: forall r. (IonScroll -> Effect Unit) -> Poll (Attribute (onIonScroll :: IonScroll | r))
onIonScroll_ = pure >>> onIonScroll

onIonScrollStart_
  :: forall r. (IonScrollStart -> Effect Unit) -> Poll (Attribute (onIonScrollStart :: IonScrollStart | r))
onIonScrollStart_ = pure >>> onIonScrollStart

onIonScrollEnd_
  :: forall r. (IonScrollEnd -> Effect Unit) -> Poll (Attribute (onIonScrollEnd :: IonScrollEnd | r))
onIonScrollEnd_ = pure >>> onIonScrollEnd

-- Event listener definitions using callbackWithCaution
onIonScroll
  :: forall r. Poll (IonScroll -> Effect Unit) -> Poll (Attribute (onIonScroll :: IonScroll | r))
onIonScroll = map ((coerce :: _ -> _ -> _ Unit) >>> map (_ $> true) >>> callbackWithCaution "ionScroll")

onIonScrollStart
  :: forall r. Poll (IonScrollStart -> Effect Unit) -> Poll (Attribute (onIonScrollStart :: IonScrollStart | r))
onIonScrollStart = map ((coerce :: _ -> _ -> _ Unit) >>> map (_ $> true) >>> callbackWithCaution "ionScrollStart")

onIonScrollEnd
  :: forall r. Poll (IonScrollEnd -> Effect Unit) -> Poll (Attribute (onIonScrollEnd :: IonScrollEnd | r))
onIonScrollEnd = map ((coerce :: _ -> _ -> _ Unit) >>> map (_ $> true) >>> callbackWithCaution "ionScrollEnd")

-- Foreign imports remain unchanged
foreign import getScrollElement :: IonContent -> Effect (Promise Web.HTML.HTMLElement)
foreign import scrollByPoint
  :: IonContent -> Number -> Number -> Number -> Effect (Promise Unit)

foreign import scrollToBottom :: IonContent -> Number -> Effect (Promise Unit)
foreign import scrollToPoint
  :: IonContent -> Number -> Number -> Number -> Effect (Promise Unit)

foreign import scrollToTop :: IonContent -> Number -> Effect (Promise Unit)
