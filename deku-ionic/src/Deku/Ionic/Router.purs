module Deku.Ionic.Router where

import Prelude

import Control.Plus (empty)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Deku.Attribute (Attribute)
import Deku.Core (Nut, elementify, attributeAtYourOwnRisk, callbackWithCaution)
import Deku.DOM (HTMLElement)
import Deku.DOM.Self (class IsSelf)
import Deku.Ionic.Enums as E
import Deku.Ionic.Unsafe as U
import Effect (Effect)
import FRP.Poll (Poll)
import Safe.Coerce (coerce)
import Type.Proxy (Proxy)
import Web.Event.Internal.Types as Web.Event.Internal.Types

data IonRouter

type HTMLIonRouter (r :: Row Type) =
  ( __tag :: Proxy "HTMLIonRouter"
  , root :: String
  , useHash :: Boolean
  , ionRouteDidChange :: IonRouteDidChange
  , ionRouteWillChange :: IonRouteWillChange
  | HTMLElement r
  )

newtype IonRouteDidChange = IonRouteDidChange Web.Event.Internal.Types.Event
newtype IonRouteWillChange = IonRouteWillChange Web.Event.Internal.Types.Event

instance IsSelf IonRouter "HTMLIonRouter"

ionRouter
  :: Array (Poll (Attribute (HTMLIonRouter ()))) -> Array Nut -> Nut
ionRouter = elementify Nothing "ion-router"

ionRouter_
  :: Array Nut
  -> Nut
ionRouter_ = ionRouter empty

root
  :: forall r
   . Poll String
  -> Poll (Attribute (root :: String | r))
root = map (attributeAtYourOwnRisk "root")

useHash
  :: forall r
   . Poll Boolean
  -> Poll (Attribute (useHash :: Boolean | r))
useHash = map (show >>> attributeAtYourOwnRisk "use-hash")

ionRouteDidChange
  :: forall r
   . Poll (IonRouteDidChange -> Effect Unit)
  -> Poll (Attribute (ionRouteDidChange :: IonRouteDidChange | r))
ionRouteDidChange = map ((coerce :: _ -> _ -> _ Unit) >>> map (_ $> true) >>> callbackWithCaution "ionRouteDidChange")

ionRouteWillChange
  :: forall r
   . Poll (IonRouteWillChange -> Effect Unit)
  -> Poll (Attribute (ionRouteWillChange :: IonRouteWillChange | r))
ionRouteWillChange = map ((coerce :: _ -> _ -> _ Unit) >>> map (_ $> true) >>> callbackWithCaution "ionRouteWillChange")

-- Direct assignment functions for attributes in IonRouter

root_
  :: forall r
   . String
  -> Poll (Attribute (root :: String | r))
root_ = pure >>> root

useHash_
  :: forall r
   . Boolean
  -> Poll (Attribute (useHash :: Boolean | r))
useHash_ = pure >>> useHash

-- Direct assignment functions for event listeners in IonRouter

ionRouteDidChange_
  :: forall r
   . (IonRouteDidChange -> Effect Unit)
  -> Poll (Attribute (ionRouteDidChange :: IonRouteDidChange | r))
ionRouteDidChange_ = pure >>> ionRouteDidChange

ionRouteWillChange_
  :: forall r
   . (IonRouteWillChange -> Effect Unit)
  -> Poll (Attribute (ionRouteWillChange :: IonRouteWillChange | r))
ionRouteWillChange_ = pure >>> ionRouteWillChange

foreign import back :: IonRouter -> Effect Unit
foreign import push
  :: IonRouter
  -> String
  -> Nullable E.RouterDirection
  -> Nullable U.AnimationBuilder
  -> Effect Unit
