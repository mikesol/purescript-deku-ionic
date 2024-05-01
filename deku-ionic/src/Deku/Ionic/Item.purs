module Deku.Ionic.Item where

import Prelude

import Control.Plus (empty)
import Data.Function.Uncurried (Fn2)
import Data.Maybe (Maybe(..))
import Deku.Attribute (Attribute)
import Deku.Core (Nut, elementify, attributeAtYourOwnRisk)
import Deku.DOM (HTMLElement)
import Deku.DOM.Self (class IsSelf)
import Deku.Ionic.Enums as E
import Deku.Ionic.Unsafe (RouterAnimation)
import Effect (Effect)
import FRP.Poll (Poll)
import Type.Proxy (Proxy)

data IonItem

type HTMLIonItem (r :: Row Type) =
  ( __tag :: Proxy "HTMLIonItem"
  , button :: Boolean
  , color :: E.Color
  , counter :: Boolean
  , detail :: Boolean
  , detailIcon :: String
  , disabled :: Boolean
  , download :: String
  , fill :: E.Fill
  , href :: String
  , lines :: E.Lines
  , mode :: E.Mode
  , rel :: String
  , routerDirection :: E.RouterDirection
  , shape :: E.ItemShape
  , target :: String
  , xtype :: E.ItemType
  | HTMLElement r
  )

instance IsSelf IonItem "HTMLIonItem"

ionItem
  :: Array (Poll (Attribute (HTMLIonItem ()))) -> Array Nut -> Nut
ionItem = elementify Nothing "ion-item"

ionItem_
  :: Array Nut
  -> Nut
ionItem_ = ionItem empty

-- Attribute handling functions with underscore versions for direct assignment
button
  :: forall r
   . Poll Boolean
  -> Poll (Attribute (button :: Boolean | r))
button = map (show >>> attributeAtYourOwnRisk "button")

button_ :: forall r. Boolean -> Poll (Attribute (button :: Boolean | r))
button_ = pure >>> button

color
  :: forall r
   . Poll E.Color
  -> Poll (Attribute (color :: E.Color | r))
color = map (E.unColor >>> attributeAtYourOwnRisk "color")

color_ :: forall r. E.Color -> Poll (Attribute (color :: E.Color | r))
color_ = pure >>> color

counter
  :: forall r
   . Poll Boolean
  -> Poll (Attribute (counter :: Boolean | r))
counter = map (show >>> attributeAtYourOwnRisk "counter")

counter_ :: forall r. Boolean -> Poll (Attribute (counter :: Boolean | r))
counter_ = pure >>> counter

detail
  :: forall r
   . Poll Boolean
  -> Poll (Attribute (detail :: Boolean | r))
detail = map (show >>> attributeAtYourOwnRisk "detail")

detail_ :: forall r. Boolean -> Poll (Attribute (detail :: Boolean | r))
detail_ = pure >>> detail

detailIcon
  :: forall r
   . Poll String
  -> Poll (Attribute (detailIcon :: String | r))
detailIcon = map (attributeAtYourOwnRisk "detail-icon")

detailIcon_ :: forall r. String -> Poll (Attribute (detailIcon :: String | r))
detailIcon_ = pure >>> detailIcon

disabled
  :: forall r
   . Poll Boolean
  -> Poll (Attribute (disabled :: Boolean | r))
disabled = map (show >>> attributeAtYourOwnRisk "disabled")

disabled_ :: forall r. Boolean -> Poll (Attribute (disabled :: Boolean | r))
disabled_ = pure >>> disabled

download
  :: forall r
   . Poll String
  -> Poll (Attribute (download :: String | r))
download = map (attributeAtYourOwnRisk "download")

download_ :: forall r. String -> Poll (Attribute (download :: String | r))
download_ = pure >>> download

fill
  :: forall r
   . Poll E.Fill
  -> Poll (Attribute (fill :: E.Fill | r))
fill = map (E.unFill >>> attributeAtYourOwnRisk "fill")

fill_ :: forall r. E.Fill -> Poll (Attribute (fill :: E.Fill | r))
fill_ = pure >>> fill

href
  :: forall r
   . Poll String
  -> Poll (Attribute (href :: String | r))
href = map (attributeAtYourOwnRisk "href")

href_ :: forall r. String -> Poll (Attribute (href :: String | r))
href_ = pure >>> href

lines
  :: forall r
   . Poll E.Lines
  -> Poll (Attribute (lines :: E.Lines | r))
lines = map (E.unLines >>> attributeAtYourOwnRisk "lines")

lines_ :: forall r. E.Lines -> Poll (Attribute (lines :: E.Lines | r))
lines_ = pure >>> lines

mode
  :: forall r
   . Poll E.Mode
  -> Poll (Attribute (mode :: E.Mode | r))
mode = map (E.unMode >>> attributeAtYourOwnRisk "mode")

mode_ :: forall r. E.Mode -> Poll (Attribute (mode :: E.Mode | r))
mode_ = pure >>> mode

rel
  :: forall r
   . Poll String
  -> Poll (Attribute (rel :: String | r))
rel = map (attributeAtYourOwnRisk "rel")

rel_ :: forall r. String -> Poll (Attribute (rel :: String | r))
rel_ = pure >>> rel

routerDirection
  :: forall r
   . Poll E.RouterDirection
  -> Poll (Attribute (routerDirection :: E.RouterDirection | r))
routerDirection = map (E.unRouterDirection >>> attributeAtYourOwnRisk "router-direction")

routerDirection_ :: forall r. E.RouterDirection -> Poll (Attribute (routerDirection :: E.RouterDirection | r))
routerDirection_ = pure >>> routerDirection

shape
  :: forall r
   . Poll E.ItemShape
  -> Poll (Attribute (shape :: E.ItemShape | r))
shape = map (E.unItemShape >>> attributeAtYourOwnRisk "shape")

shape_ :: forall r. E.ItemShape -> Poll (Attribute (shape :: E.ItemShape | r))
shape_ = pure >>> shape

target
  :: forall r
   . Poll String
  -> Poll (Attribute (target :: String | r))
target = map (attributeAtYourOwnRisk "target")

target_ :: forall r. String -> Poll (Attribute (target :: String | r))
target_ = pure >>> target

xtype
  :: forall r
   . Poll E.ItemType
  -> Poll (Attribute (xtype :: E.ItemType | r))
xtype = map (E.unItemType >>> attributeAtYourOwnRisk "type")

xtype_ :: forall r. E.ItemType -> Poll (Attribute (xtype :: E.ItemType | r))
xtype_ = pure >>> xtype

type CounterFormatter = Fn2 Number Number String

foreign import counterFormatter :: IonItem -> CounterFormatter -> Effect Unit
foreign import getCounterFormatter :: IonItem -> Effect CounterFormatter
foreign import routerAnimation :: IonItem -> RouterAnimation -> Effect Unit
foreign import getRouterAnimation :: IonItem -> Effect RouterAnimation