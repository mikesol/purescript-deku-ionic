module Deku.Ionic.SelectOption where

import Prelude 

import Control.Plus (empty)
import Data.Maybe (Maybe(..))
import Deku.Attribute (Attribute)
import Deku.Core (Nut, attributeAtYourOwnRisk, elementify)
import Deku.DOM (HTMLElement)
import Deku.DOM.Self (class IsSelf)
import FRP.Poll (Poll)
import Type.Proxy (Proxy)

data IonSelectOption

type HTMLIonSelectOption (r :: Row Type) =
  ( __tag :: Proxy "HTMLIonSelectOption"
  , disabled :: Boolean
  , value :: String
  | HTMLElement r
  )

instance IsSelf IonSelectOption "HTMLIonSelectOption"
disabled
  :: forall r. Poll Boolean -> Poll (Attribute (disabled :: Boolean | r))
disabled = map (show >>> attributeAtYourOwnRisk "disabled")

disabled_
  :: forall r. Boolean -> Poll (Attribute (disabled :: Boolean | r))
disabled_ = pure >>> disabled

value
  :: forall r. Poll String -> Poll (Attribute (value :: String | r))
value = map (attributeAtYourOwnRisk "value")

value_
  :: forall r. String -> Poll (Attribute (value :: String | r))
value_ = pure >>> value

ionSelectOption
  :: Array (Poll (Attribute (HTMLIonSelectOption ()))) -> Array Nut -> Nut
ionSelectOption = elementify Nothing "ion-select-option"

ionSelectOption_
  :: Array Nut -> Nut
ionSelectOption_ = ionSelectOption empty
