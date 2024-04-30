module Deku.Ionic.Route where

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
import Control.Promise (Promise)
import Foreign (Foreign)
import Foreign.Object (Object)
import Untagged.Union (type (|+|), UndefinedOr)

data IonRoute

newtype IonRouteDataChanged = IonRouteDataChanged Web.Event.Internal.Types.Event
newtype IonRouteWillChange = IonRouteWillChange Web.Event.Internal.Types.Event

type HTMLIonRoute (r :: Row Type) =
  ( __tag :: Proxy "HTMLIonRoute"
  , component :: String
  , url :: String
  , ionRouteDataChanged :: IonRouteDataChanged
  , ionRouteWillChange :: IonRouteWillChange
  | HTMLElement r
  )

instance IsSelf IonRoute "HTMLIonRoute"

ionRoute
  :: Array (Poll (Attribute (HTMLIonRoute ()))) -> Array Nut -> Nut
ionRoute = elementify Nothing "ion-route"

ionRoute_
  :: Array Nut
  -> Nut
ionRoute_ = ionRoute empty

component
  :: forall r. Poll String
  -> Poll (Attribute (component :: String | r))
component = map (attributeAtYourOwnRisk "component")

url
  :: forall r. Poll String
  -> Poll (Attribute (url :: String | r))
url = map (attributeAtYourOwnRisk "url")

ionRouteDataChanged
  :: forall r. Poll (IonRouteDataChanged -> Effect Unit)
  -> Poll (Attribute (ionRouteDataChanged :: IonRouteDataChanged | r))
ionRouteDataChanged = map ((coerce :: _ -> _ -> _ Unit) >>> map (_ $> true) >>> callbackWithCaution "ionRouteDataChanged")

ionRouteWillChange
  :: forall r. Poll (IonRouteWillChange -> Effect Unit)
  -> Poll (Attribute (ionRouteWillChange :: IonRouteWillChange | r))
ionRouteWillChange = map ((coerce :: _ -> _ -> _ Unit) >>> map (_ $> true) >>> callbackWithCaution "ionRouteWillChange")

-- Direct assignment functions for attributes and listeners in IonRoute

component_
  :: forall r
   . String
  -> Poll (Attribute (component :: String | r))
component_ = pure >>> component

url_
  :: forall r
   . String
  -> Poll (Attribute (url :: String | r))
url_ = pure >>> url

ionRouteDataChanged_
  :: forall r
   . (IonRouteDataChanged -> Effect Unit)
  -> Poll (Attribute (ionRouteDataChanged :: IonRouteDataChanged | r))
ionRouteDataChanged_ = pure >>> ionRouteDataChanged

ionRouteWillChange_
  :: forall r
   . (IonRouteWillChange -> Effect Unit)
  -> Poll (Attribute (ionRouteWillChange :: IonRouteWillChange | r))
ionRouteWillChange_ = pure >>> ionRouteWillChange

type NavigationHookResult = Boolean |+|
  { redirect :: String
  }

foreign import beforeEnter
  :: IonRoute
  -> UndefinedOr (Effect (Promise NavigationHookResult))
  -> Effect Unit

foreign import beforeLeave
  :: IonRoute
  -> UndefinedOr (Effect (Promise NavigationHookResult))
  -> Effect Unit

foreign import componentProps
  :: IonRoute -> UndefinedOr (Object Foreign) -> Effect Unit

foreign import getBeforeEnter
  :: IonRoute -> Effect (UndefinedOr (Effect (Promise NavigationHookResult)))

foreign import getBeforeLeave
  :: IonRoute -> Effect (UndefinedOr (Effect (Promise NavigationHookResult)))

foreign import getComponentProps
  :: IonRoute -> Effect (UndefinedOr (Object Foreign))
