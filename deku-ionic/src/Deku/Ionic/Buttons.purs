module Deku.Ionic.Buttons where

import Prelude hiding (max, min)

import Control.Plus (empty)
import Data.Maybe (Maybe(..))
import Deku.Attribute (Attribute)
import Deku.Core (Nut, attributeAtYourOwnRisk, elementify)
import Deku.DOM (HTMLElement)
import Deku.DOM.Self (class IsSelf)
import FRP.Poll (Poll)
import Type.Proxy (Proxy)

data IonButtons

type HTMLIonButtons (r :: Row Type) =
  ( __tag :: Proxy "HTMLIonButtons"
  , slot :: String
  , collapse :: Boolean
  | HTMLElement r
  )

instance IsSelf IonButtons "HTMLIonButtons"

ionButtons
  :: Array (Poll (Attribute (HTMLIonButtons ()))) -> Array Nut -> Nut
ionButtons = elementify Nothing "ion-buttons"

ionButtons_
  :: Array Nut -> Nut
ionButtons_ = ionButtons empty

-- Attribute functions for Slot and Collapse
slot
  :: forall r. Poll String -> Poll (Attribute (slot :: String | r))
slot = map (attributeAtYourOwnRisk "slot")

slot_
  :: forall r. String -> Poll (Attribute (slot :: String | r))
slot_ = pure >>> slot

collapse
  :: forall r. Poll Boolean -> Poll (Attribute (collapse :: Boolean | r))
collapse = map (show >>> attributeAtYourOwnRisk "collapse")

collapse_
  :: forall r. Boolean -> Poll (Attribute (collapse :: Boolean | r))
collapse_ = pure >>> collapse
