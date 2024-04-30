module Deku.Ionic.Nav where

import Prelude

import Control.Plus (empty)
import Data.Maybe (Maybe(..))
import Deku.Attribute (Attribute)
import Deku.Core (Nut, elementify, attributeAtYourOwnRisk, callbackWithCaution)
import Deku.DOM (HTMLElement)
import Deku.DOM.Self (class IsSelf)
import Effect (Effect)
import FRP.Poll (Poll)
import Safe.Coerce (coerce)
import Type.Proxy (Proxy)
import Web.Event.Internal.Types as Web.Event.Internal.Types

data IonNav

type HTMLIonNav (r :: Row Type) =
  ( __tag :: Proxy "HTMLIonNav"
  , animated :: Boolean
  , swipeGesture :: Boolean
  , ionNavDidChange :: IonNavDidChange
  , ionNavWillChange :: IonNavWillChange
  | HTMLElement r
  )

newtype IonNavDidChange = IonNavDidChange Web.Event.Internal.Types.Event
newtype IonNavWillChange = IonNavWillChange Web.Event.Internal.Types.Event

instance IsSelf IonNav "HTMLIonNav"

ionNav
  :: Array (Poll (Attribute (HTMLIonNav ()))) -> Array Nut -> Nut
ionNav = elementify Nothing "ion-nav"

ionNav_
  :: Array Nut
  -> Nut
ionNav_ = ionNav empty

animated
  :: forall r. Poll Boolean
  -> Poll (Attribute (animated :: Boolean | r))
animated = map (show >>> attributeAtYourOwnRisk "animated")

swipeGesture
  :: forall r. Poll Boolean
  -> Poll (Attribute (swipeGesture :: Boolean | r))
swipeGesture = map (show >>> attributeAtYourOwnRisk "swipe-gesture")

ionNavDidChange
  :: forall r. Poll (IonNavDidChange -> Effect Unit)
  -> Poll (Attribute (ionNavDidChange :: IonNavDidChange | r))
ionNavDidChange = map ((coerce :: _ -> _ -> _ Unit) >>> map (_ $> true) >>> callbackWithCaution "ionNavDidChange")

ionNavWillChange
  :: forall r. Poll (IonNavWillChange -> Effect Unit)
  -> Poll (Attribute (ionNavWillChange :: IonNavWillChange | r))
ionNavWillChange = map ((coerce :: _ -> _ -> _ Unit) >>> map (_ $> true) >>> callbackWithCaution "ionNavWillChange")

-- Direct assignment functions for attributes and listeners in IonNav

animated_
  :: forall r
   . Boolean
  -> Poll (Attribute (animated :: Boolean | r))
animated_ = pure >>> animated

swipeGesture_
  :: forall r
   . Boolean
  -> Poll (Attribute (swipeGesture :: Boolean | r))
swipeGesture_ = pure >>> swipeGesture

ionNavDidChange_
  :: forall r
   . (IonNavDidChange -> Effect Unit)
  -> Poll (Attribute (ionNavDidChange :: IonNavDidChange | r))
ionNavDidChange_ = pure >>> ionNavDidChange

ionNavWillChange_
  :: forall r
   . (IonNavWillChange -> Effect Unit)
  -> Poll (Attribute (ionNavWillChange :: IonNavWillChange | r))
ionNavWillChange_ = pure >>> ionNavWillChange
