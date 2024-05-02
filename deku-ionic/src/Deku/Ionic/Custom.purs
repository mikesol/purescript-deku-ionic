module Deku.Ionic.Custom where

import Prelude

import Deku.Core (Nut)
import Deku.Toplevel (runInElement)
import Effect (Effect)
import Web.DOM.Element as Web.DOM

foreign import customComponentImpl
  :: String
  -> Effect Unit
  -> Effect Unit
  -> (Web.DOM.Element -> Effect Unit)
  -> Effect Unit

customComponent
  :: String
  -> Effect Unit
  -> Effect Unit
  -> Nut
  -> Effect Unit
customComponent s r k nut = customComponentImpl s r k
  \e ->
    runInElement e nut

customComponent_
  :: String
  -> Nut
  -> Effect Unit
customComponent_ s = customComponent s mempty  mempty 
