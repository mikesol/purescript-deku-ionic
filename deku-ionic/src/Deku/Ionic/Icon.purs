module Deku.Ionic.Icon where

import Prelude

import Control.Plus (empty)
import Data.Maybe (Maybe(..))
import Deku.Attribute (Attribute)
import Deku.Core (Nut, elementify, attributeAtYourOwnRisk)
import Deku.DOM (HTMLElement)
import Deku.DOM.Self (class IsSelf)
import Deku.Ionic.Enums as E
import FRP.Poll (Poll)
import Type.Proxy (Proxy)

data IonIcon

type HTMLIonIcon (r :: Row Type) =
  ( __tag :: Proxy "HTMLIonIcon"
  , name :: String
  , slot :: String
  , src :: String
  , size :: E.Size
  | HTMLElement r
  )

instance IsSelf IonIcon "HTMLIonIcon"

ionIcon
  :: Array (Poll (Attribute (HTMLIonIcon ()))) -> Array Nut -> Nut
ionIcon = elementify Nothing "ion-icon"

ionIcon_
  :: Array Nut
  -> Nut
ionIcon_ = ionIcon empty

-- Attribute handling functions
size
  :: forall r
   . Poll String
  -> Poll (Attribute (size :: String | r))
size = map (attributeAtYourOwnRisk "size")

size_
  :: forall r
   . String
  -> Poll (Attribute (size :: String | r))
size_ = pure >>> size

src
  :: forall r
   . Poll String
  -> Poll (Attribute (src :: String | r))
src = map (attributeAtYourOwnRisk "src")

src_
  :: forall r
   . String
  -> Poll (Attribute (src :: String | r))
src_ = pure >>> src

name
  :: forall r
   . Poll String
  -> Poll (Attribute (name :: String | r))
name = map (attributeAtYourOwnRisk "name")

name_
  :: forall r
   . String
  -> Poll (Attribute (name :: String | r))
name_ = pure >>> name

slot
  :: forall r
   . Poll String
  -> Poll (Attribute (slot :: String | r))
slot = map (attributeAtYourOwnRisk "slot")

slot_
  :: forall r
   . String
  -> Poll (Attribute (slot :: String | r))
slot_ = pure >>> slot
