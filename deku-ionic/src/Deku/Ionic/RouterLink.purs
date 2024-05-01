module Deku.Ionic.RouterLink where

import Prelude

import Control.Plus (empty)
import Data.Maybe (Maybe(..))
import Deku.Attribute (Attribute)
import Deku.Core (Nut, elementify, attributeAtYourOwnRisk)
import Deku.DOM (HTMLElement)
import Deku.DOM.Self (class IsSelf)
import Deku.Ionic.Enums as E
import Deku.Ionic.Unsafe (RouterAnimation)
import Effect (Effect)
import FRP.Poll (Poll)
import Type.Proxy (Proxy)
import Untagged.Union (UndefinedOr)


data IonRouterLink

type HTMLIonRouterLink (r :: Row Type) =
  ( __tag :: Proxy "HTMLIonRouterLink"
  , color :: E.Color
  , href :: String
  , rel :: String
  , routerDirection :: String
  , target :: String
  | HTMLElement r
  )

instance IsSelf IonRouterLink "HTMLIonRouterLink"

ionRouterLink
  :: Array (Poll (Attribute (HTMLIonRouterLink ()))) -> Array Nut -> Nut
ionRouterLink = elementify Nothing "ion-router-link"

ionRouterLink_
  :: Array Nut
  -> Nut
ionRouterLink_ = ionRouterLink empty

color
  :: forall r
   . Poll E.Color
  -> Poll (Attribute (color :: E.Color | r))
color = map (E.unColor >>> attributeAtYourOwnRisk "color")

href
  :: forall r
   . Poll String
  -> Poll (Attribute (href :: String | r))
href = map (attributeAtYourOwnRisk "href")

rel
  :: forall r
   . Poll String
  -> Poll (Attribute (rel :: String | r))
rel = map (attributeAtYourOwnRisk "rel")

routerDirection
  :: forall r
   . Poll E.RouterDirection
  -> Poll (Attribute (routerDirection :: String | r))
routerDirection = map (E.unRouterDirection >>> attributeAtYourOwnRisk "router-direction")

target
  :: forall r
   . Poll String
  -> Poll (Attribute (target :: String | r))
target = map (attributeAtYourOwnRisk "target")

-- Direct assignment functions for attributes in IonRouterLink

color_
  :: forall r
   . E.Color
  -> Poll (Attribute (color :: E.Color | r))
color_ = pure >>> color

href_
  :: forall r
   . String
  -> Poll (Attribute (href :: String | r))
href_ = pure >>> href

rel_
  :: forall r
   . String
  -> Poll (Attribute (rel :: String | r))
rel_ = pure >>> rel

routerDirection_
  :: forall r
   . E.RouterDirection
  -> Poll (Attribute (routerDirection :: String | r))
routerDirection_ = pure >>> routerDirection

target_
  :: forall r
   . String
  -> Poll (Attribute (target :: String | r))
target_ = pure >>> target

foreign import routerAnimation
  :: IonRouterLink -> UndefinedOr RouterAnimation -> Effect Unit

foreign import getRouterAnimation
  :: IonRouterLink -> Effect (UndefinedOr RouterAnimation)
