module Deku.Ionic.Grid where

import Prelude

import Control.Plus (empty)
import Data.Maybe (Maybe(..))
import Deku.Attribute (Attribute)
import Deku.Core (Nut, elementify, attributeAtYourOwnRisk)
import Deku.DOM (HTMLElement)
import Deku.DOM.Self (class IsSelf)
import FRP.Poll (Poll)
import Type.Proxy (Proxy)

data IonGrid

type HTMLIonGrid (r :: Row Type) =
  ( __tag :: Proxy "HTMLIonGrid"
  , fixed :: Boolean
  | HTMLElement r
  )

instance IsSelf IonGrid "HTMLIonGrid"

ionGrid
  :: Array (Poll (Attribute (HTMLIonGrid ()))) -> Array Nut -> Nut
ionGrid = elementify Nothing "ion-grid"

ionGrid_
  :: Array Nut
  -> Nut
ionGrid_ = ionGrid empty

-- Attribute handling functions with underscore versions for direct assignment
fixed
  :: forall r
   . Poll Boolean
  -> Poll (Attribute (fixed :: Boolean | r))
fixed = map (show >>> attributeAtYourOwnRisk "fixed")

fixed_
  :: forall r
   . Boolean
  -> Poll (Attribute (fixed :: Boolean | r))
fixed_ = pure >>> fixed
