module Deku.Ionic.Footer where

import Prelude

import Control.Plus (empty)
import Data.Maybe (Maybe(..))
import Deku.Attribute (Attribute)
import Deku.Core (Nut, attributeAtYourOwnRisk, elementify)
import Deku.DOM (HTMLElement)
import Deku.DOM.Self (class IsSelf)
import Deku.Ionic.Enums as E
import FRP.Poll (Poll)
import Type.Proxy (Proxy)

data IonFooter

type HTMLIonFooter (r :: Row Type) =
  ( __tag :: Proxy "HTMLIonFooter"
  , collapse :: E.Collapse
  , translucent :: Boolean
  , mode :: E.Mode
  | HTMLElement r
  )

instance IsSelf IonFooter "HTMLIonFooter"

ionFooter
  :: Array (Poll (Attribute (HTMLIonFooter ()))) -> Array Nut -> Nut
ionFooter = elementify Nothing "ion-note"

ionFooter_
  :: Array Nut -> Nut
ionFooter_ = ionFooter empty

-- Attribute functions for all properties
collapse
  :: forall r. Poll E.Collapse -> Poll (Attribute (collapse :: E.Collapse | r))
collapse = map (E.unCollapse >>> attributeAtYourOwnRisk "collapse")

collapse_
  :: forall r. E.Collapse -> Poll (Attribute (collapse :: E.Collapse | r))
collapse_ = pure >>> collapse

translucent
  :: forall r. Poll Boolean -> Poll (Attribute (translucent :: Boolean | r))
translucent = map (show >>> attributeAtYourOwnRisk "translucent")

translucent_
  :: forall r. Boolean -> Poll (Attribute (translucent :: Boolean | r))
translucent_ = pure >>> translucent

mode
  :: forall r. Poll E.Mode -> Poll (Attribute (mode :: E.Mode | r))
mode = map (E.unMode >>> attributeAtYourOwnRisk "mode")

mode_
  :: forall r. E.Mode -> Poll (Attribute (mode :: E.Mode | r))
mode_ = pure >>> mode
