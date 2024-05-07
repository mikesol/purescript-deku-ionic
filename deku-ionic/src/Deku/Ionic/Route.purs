module Deku.Ionic.Route where

import Prelude

import Control.Promise (Promise)
import Data.Array (replicate)
import Data.Char (fromCharCode)
import Data.Maybe (fromMaybe)
import Data.String.CodeUnits (fromCharArray)
import Data.Traversable (sequence)
import Deku.Attribute (Attribute)
import Deku.Core (Nut, callbackWithCaution)
import Deku.DOM (HTMLElement)
import Deku.DOM.Self (class IsSelf)
import Deku.Ionic.RouterLink (HTMLIonRouterLink)
import Deku.Toplevel (runInElement)
import Effect (Effect)
import Effect.Random (randomInt)
import FRP.Poll (Poll)
import Foreign (Foreign)
import Foreign.Object (Object)
import Safe.Coerce (coerce)
import Type.Proxy (Proxy)
import Untagged.Union (type (|+|), UndefinedOr)
import Web.DOM.Element as Web.DOM
import Web.Event.Internal.Types as Web.Event.Internal.Types

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

ionRouteDataChanged
  :: forall r
   . Poll (IonRouteDataChanged -> Effect Unit)
  -> Poll (Attribute (ionRouteDataChanged :: IonRouteDataChanged | r))
ionRouteDataChanged = map ((coerce :: _ -> _ -> _ Unit) >>> map (_ $> true) >>> callbackWithCaution "ionRouteDataChanged")

ionRouteWillChange
  :: forall r
   . Poll (IonRouteWillChange -> Effect Unit)
  -> Poll (Attribute (ionRouteWillChange :: IonRouteWillChange | r))
ionRouteWillChange = map ((coerce :: _ -> _ -> _ Unit) >>> map (_ $> true) >>> callbackWithCaution "ionRouteWillChange")

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

--- custom component
foreign import unsafeCustomComponentImpl
  :: String
  -> Effect Unit
  -> Effect Unit
  -> (Web.DOM.Element -> String -> Effect Unit)
  -> Effect Unit

foreign import eagerUnsafeCustomComponentImpl
  :: String
  -> Effect Unit
  -> Effect Unit
  -> (Web.DOM.Element -> Effect Unit)
  -> Effect Unit

unsafeCustomComponent
  :: forall a
   . (String -> a)
  -> String
  -> Effect Unit
  -> Effect Unit
  -> (a -> Nut)
  -> Effect Unit
unsafeCustomComponent sf s onConnect onDisconnect nutF = unsafeCustomComponentImpl s onConnect onDisconnect
  \e str ->
    runInElement e (nutF $ sf str)

eagerUnsafeCustomComponent
  :: String
  -> Effect Unit
  -> Effect Unit
  -> Nut
  -> Effect Unit
eagerUnsafeCustomComponent s onConnect onDisconnect nut = eagerUnsafeCustomComponentImpl s onConnect onDisconnect
  \e ->
    runInElement e nut


-- Function to generate a random lowercase letter
randomLetter :: Effect Char
randomLetter = do
  index <- randomInt 97 122
  pure $ fromMaybe 'z' $ fromCharCode index

-- Function to generate a part of the pattern (either before or after the dash)
randomLetters :: Int -> Effect String
randomLetters n = do
  letters <- sequence $ replicate n randomLetter
  pure $ fromCharArray letters

-- Function to generate the full pattern "xxxxx-xxxxx"
generateEltName :: Effect String
generateEltName = do
  firstPart <- randomLetters 5
  secondPart <- randomLetters 5
  pure $ firstPart <> "-" <> secondPart

type IonRouteSig r =
       { route :: Array (Poll (Attribute (HTMLIonRoute ()))) -> Nut
       , link :: Poll r -> Array (Poll (Attribute (HTMLIonRouterLink ()))) -> Array Nut -> Nut
       }
