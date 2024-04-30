module Deku.Ionic.App where

import Control.Plus (empty)
import Data.Maybe (Maybe(..))
import Deku.Core (Attribute, Nut, elementify)
import Deku.DOM (HTMLElement)
import Deku.DOM.Self (class IsSelf)
import FRP.Poll (Poll)
import Type.Proxy (Proxy)

data IonApp

type HTMLIonApp (r :: Row Type) =
  ( __tag :: Proxy "HTMLIonApp"
  | HTMLElement r
  )

instance IsSelf IonApp "HTMLIonApp"

ionApp
  :: Array (Poll (Attribute (HTMLIonApp ()))) -> Array Nut -> Nut
ionApp = elementify Nothing "ion-app"

ionApp_
  :: Array Nut -> Nut
ionApp_ = ionApp empty
