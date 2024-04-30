module Deku.Ionic.Header where

import Prelude

import Control.Plus (empty)
import Data.Maybe (Maybe(..))
import Deku.Attribute (Attribute)
import Deku.Core (Nut, elementify, attributeAtYourOwnRisk)
import Deku.DOM (HTMLElement)
import Deku.DOM.Self (class IsSelf)
import FRP.Poll (Poll)
import Type.Proxy (Proxy)
import Deku.Ionic.Enums as E

data IonHeader

type HTMLIonHeader (r :: Row Type) =
  ( __tag :: Proxy "HTMLIonHeader"
  , collapse :: E.Collapse
  , mode :: E.Mode
  , translucent :: Boolean
  | HTMLElement r
  )

instance IsSelf IonHeader "HTMLIonHeader"

ionHeader
  :: Array (Poll (Attribute (HTMLIonHeader ()))) -> Array Nut -> Nut
ionHeader = elementify Nothing "ion-header"

ionHeader_
  :: Array Nut
  -> Nut
ionHeader_ = ionHeader empty

-- Attribute handling functions with underscore versions for direct assignment
collapse
  :: forall r
   . Poll E.Collapse
  -> Poll (Attribute (collapse :: E.Collapse | r))
collapse = map (E.unCollapse >>> attributeAtYourOwnRisk "collapse")

collapse_
  :: forall r
   . E.Collapse
  -> Poll (Attribute (collapse :: E.Collapse | r))
collapse_ = pure >>> collapse

mode
  :: forall r
   . Poll E.Mode
  -> Poll (Attribute (mode :: E.Mode | r))
mode = map (E.unMode >>> attributeAtYourOwnRisk "mode")

mode_
  :: forall r
   . E.Mode
  -> Poll (Attribute (mode :: E.Mode | r))
mode_ = pure >>> mode

translucent
  :: forall r
   . Poll Boolean
  -> Poll (Attribute (translucent :: Boolean | r))
translucent = map (show >>> attributeAtYourOwnRisk "translucent")

translucent_
  :: forall r
   . Boolean
  -> Poll (Attribute (translucent :: Boolean | r))
translucent_ = pure >>> translucent
