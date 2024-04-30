module Deku.Ionic.Card where

import Prelude hiding (max, min)

import Control.Plus (empty)
import Data.Maybe (Maybe(..))
import Deku.Attribute (Attribute)
import Deku.Core (Nut, attributeAtYourOwnRisk, elementify)
import Deku.DOM (HTMLElement)
import Deku.DOM.Self (class IsSelf)
import Deku.Ionic.Enums as E
import Deku.Ionic.Unsafe (RouterAnimation)
import Effect (Effect)
import FRP.Poll (Poll)
import Type.Proxy (Proxy)

data IonCard

type HTMLIonCard (r :: Row Type) =
  ( __tag :: Proxy "HTMLIonCard"
  , slot :: String
  , button :: Boolean
  , color :: E.Color
  , disabled :: Boolean
  , download :: String
  , href :: String
  , mode :: E.Mode
  , rel :: String
  , routerDirection :: E.RouterDirection
  , target :: String
  , xtype :: E.ItemType
  | HTMLElement r
  )

instance IsSelf IonCard "HTMLIonCard"

ionCard
  :: Array (Poll (Attribute (HTMLIonCard ()))) -> Array Nut -> Nut
ionCard = elementify Nothing "ion-card"

ionCard_
  :: Array Nut -> Nut
ionCard_ = ionCard empty

-- Attribute functions for all properties
slot
  :: forall r. Poll String -> Poll (Attribute (slot :: String | r))
slot = map (attributeAtYourOwnRisk "slot")

slot_
  :: forall r. String -> Poll (Attribute (slot :: String | r))
slot_ = pure >>> slot

button
  :: forall r. Poll Boolean -> Poll (Attribute (button :: Boolean | r))
button = map (show >>> attributeAtYourOwnRisk "button")

button_
  :: forall r. Boolean -> Poll (Attribute (button :: Boolean | r))
button_ = pure >>> button

color
  :: forall r. Poll E.Color -> Poll (Attribute (color :: E.Color | r))
color = map (E.unColor >>> attributeAtYourOwnRisk "color")

color_
  :: forall r. E.Color -> Poll (Attribute (color :: E.Color | r))
color_ = pure >>> color

disabled
  :: forall r. Poll Boolean -> Poll (Attribute (disabled :: Boolean | r))
disabled = map (show >>> attributeAtYourOwnRisk "disabled")

disabled_
  :: forall r. Boolean -> Poll (Attribute (disabled :: Boolean | r))
disabled_ = pure >>> disabled

download
  :: forall r. Poll String -> Poll (Attribute (download :: String | r))
download = map (attributeAtYourOwnRisk "download")

download_
  :: forall r. String -> Poll (Attribute (download :: String | r))
download_ = pure >>> download

href
  :: forall r. Poll String -> Poll (Attribute (href :: String | r))
href = map (attributeAtYourOwnRisk "href")

href_
  :: forall r. String -> Poll (Attribute (href :: String | r))
href_ = pure >>> href

mode
  :: forall r. Poll E.Mode -> Poll (Attribute (mode :: E.Mode | r))
mode = map (E.unMode >>> attributeAtYourOwnRisk "mode")

mode_
  :: forall r. E.Mode -> Poll (Attribute (mode :: E.Mode | r))
mode_ = pure >>> mode

rel
  :: forall r. Poll String -> Poll (Attribute (rel :: String | r))
rel = map (attributeAtYourOwnRisk "rel")

rel_
  :: forall r. String -> Poll (Attribute (rel :: String | r))
rel_ = pure >>> rel

routerDirection
  :: forall r. Poll E.RouterDirection -> Poll (Attribute (routerDirection :: E.RouterDirection | r))
routerDirection = map (E.unRouterDirection >>> attributeAtYourOwnRisk "router-direction")

routerDirection_
  :: forall r. E.RouterDirection -> Poll (Attribute (routerDirection :: E.RouterDirection | r))
routerDirection_ = pure >>> routerDirection

target
  :: forall r. Poll String -> Poll (Attribute (target :: String | r))
target = map (attributeAtYourOwnRisk "target")

target_
  :: forall r. String -> Poll (Attribute (target :: String | r))
target_ = pure >>> target

xtype
  :: forall r. Poll E.ItemType -> Poll (Attribute (xtype :: E.ItemType | r))
xtype = map (E.unItemType >>> attributeAtYourOwnRisk "type")

xtype_
  :: forall r. E.ItemType -> Poll (Attribute (xtype :: E.ItemType | r))
xtype_ = pure >>> xtype

-- Foreign imports remain unchanged
foreign import routerAnimation :: IonCard -> RouterAnimation -> Effect Unit
foreign import getRouterAnimation :: IonCard -> Effect RouterAnimation
