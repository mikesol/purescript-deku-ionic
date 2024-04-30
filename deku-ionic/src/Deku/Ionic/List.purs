module Deku.Ionic.List where

import Prelude

import Control.Plus (empty)
import Control.Promise (Promise)
import Data.Maybe (Maybe(..))
import Deku.Attribute (Attribute)
import Deku.Core (Nut, elementify, attributeAtYourOwnRisk)
import Deku.DOM (HTMLElement)
import Deku.DOM.Self (class IsSelf)
import Effect (Effect)
import FRP.Poll (Poll)
import Type.Proxy (Proxy)
import Deku.Ionic.Enums as E

data IonList

type HTMLIonList (r :: Row Type) =
  ( __tag :: Proxy "HTMLIonList"
  , inset :: Boolean
  , lines :: E.Lines
  , mode :: E.Mode
  | HTMLElement r
  )

instance IsSelf IonList "HTMLIonList"

ionList
  :: Array (Poll (Attribute (HTMLIonList ()))) -> Array Nut -> Nut
ionList = elementify Nothing "ion-list"

ionList_
  :: Array Nut
  -> Nut
ionList_ = ionList empty

-- Attribute handling functions
inset
  :: forall r. Poll Boolean
  -> Poll (Attribute (inset :: Boolean | r))
inset = map (show >>> attributeAtYourOwnRisk "inset")

inset_
  :: forall r. Boolean
  -> Poll (Attribute (inset :: Boolean | r))
inset_ = pure >>> inset

lines
  :: forall r. Poll E.Lines
  -> Poll (Attribute (lines :: E.Lines | r))
lines = map (E.unLines >>> attributeAtYourOwnRisk "lines")

lines_
  :: forall r. E.Lines
  -> Poll (Attribute (lines :: E.Lines | r))
lines_ = pure >>> lines

mode
  :: forall r. Poll E.Mode
  -> Poll (Attribute (mode :: E.Mode | r))
mode = map (E.unMode >>> attributeAtYourOwnRisk "mode")

mode_
  :: forall r. E.Mode
  -> Poll (Attribute (mode :: E.Mode | r))
mode_ = pure >>> mode

-- Foreign import remains unchanged
foreign import closeSlidingItems :: IonList -> Effect (Promise Boolean)
