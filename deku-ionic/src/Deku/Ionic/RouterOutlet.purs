module Deku.Ionic.RouterOutlet where

import Control.Plus (empty)
import Data.Maybe (Maybe(..))
import Deku.Core (Attribute, Nut, elementify)
import Deku.DOM (HTMLElement)
import Deku.DOM.Self (class IsSelf)
import FRP.Poll (Poll)
import Type.Proxy (Proxy)

data IonRouterOutlet

type HTMLIonRouterOutlet (r :: Row Type) =
  ( __tag :: Proxy "HTMLIonRouterOutlet"
  | HTMLElement r
  )

instance IsSelf IonRouterOutlet "HTMLIonRouterOutlet"

ionRouterOutlet
  :: Array (Poll (Attribute (HTMLIonRouterOutlet ()))) -> Array Nut -> Nut
ionRouterOutlet = elementify Nothing "ion-router-outlet"

ionRouterOutlet_
  :: Array Nut -> Nut
ionRouterOutlet_ = ionRouterOutlet empty
