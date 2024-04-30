module Deku.Ionic.CardContent where

import Prelude hiding (max, min)

import Control.Plus (empty)
import Data.Maybe (Maybe(..))
import Deku.Attribute (Attribute)
import Deku.Core (Nut, attributeAtYourOwnRisk, elementify)
import Deku.DOM (HTMLElement)
import Deku.DOM.Self (class IsSelf)
import Deku.Ionic.Enums as E
import FRP.Poll (Poll)
import Type.Proxy (Proxy)

data IonCardContent

type HTMLIonCardContent (r :: Row Type) =
  ( __tag :: Proxy "HTMLIonCardContent"
  , mode :: E.Mode
  | HTMLElement r
  )

instance IsSelf IonCardContent "HTMLIonCardContent"

ionCardContent
  :: Array (Poll (Attribute (HTMLIonCardContent ()))) -> Array Nut -> Nut
ionCardContent = elementify Nothing "ion-card-content"

ionCardContent_
  :: Array Nut -> Nut
ionCardContent_ = ionCardContent empty

-- Attribute function for Mode
mode
  :: forall r. Poll E.Mode -> Poll (Attribute (mode :: E.Mode | r))
mode = map (E.unMode >>> attributeAtYourOwnRisk "mode")

mode_
  :: forall r. E.Mode -> Poll (Attribute (mode :: E.Mode | r))
mode_ = pure >>> mode
