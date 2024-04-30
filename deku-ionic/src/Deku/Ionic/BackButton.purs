module Deku.Ionic.BackButton where

import Prelude hiding (max, min)

import Control.Plus (empty)
import Data.Function.Uncurried (Fn2)
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

data IonBackButton

type HTMLIonBackButton (r :: Row Type) =
  ( __tag :: Proxy "HTMLIonBackButton"
  , slot :: String
  , color :: E.Color
  , defaultHref :: String
  , disabled :: Boolean
  , icon :: String
  , mode :: E.Mode
  , text :: String
  , xtype :: E.ItemType
  | HTMLElement r
  )

instance IsSelf IonBackButton "HTMLIonBackButton"

ionBackButton
  :: Array (Poll (Attribute (HTMLIonBackButton ()))) -> Array Nut -> Nut
ionBackButton = elementify Nothing "ion-back-button"

ionBackButton_
  :: Array Nut -> Nut
ionBackButton_ = ionBackButton empty

-- Attribute functions for the properties
slot
  :: forall r. Poll String -> Poll (Attribute (slot :: String | r))
slot = map (attributeAtYourOwnRisk "slot")

slot_
  :: forall r. String -> Poll (Attribute (slot :: String | r))
slot_ = pure >>> slot


color
  :: forall r. Poll E.Color -> Poll (Attribute (color :: E.Color | r))
color = map (E.unColor >>> attributeAtYourOwnRisk "color")

color_
  :: forall r. E.Color -> Poll (Attribute (color :: E.Color | r))

color_ = pure >>> color

defaultHref
  :: forall r. Poll String -> Poll (Attribute (defaultHref :: String | r))
defaultHref = map (attributeAtYourOwnRisk "default-href")

defaultHref_
  :: forall r. String -> Poll (Attribute (defaultHref :: String | r))
defaultHref_ = pure >>> defaultHref

disabled
  :: forall r. Poll Boolean -> Poll (Attribute (disabled :: Boolean | r))

disabled = map (show >>> attributeAtYourOwnRisk "disabled")

disabled_
  :: forall r. Boolean -> Poll (Attribute (disabled :: Boolean | r))

disabled_ = pure >>> disabled

icon
  :: forall r. Poll String -> Poll (Attribute (icon :: String | r))

icon = map (attributeAtYourOwnRisk "icon")

icon_
  :: forall r. String -> Poll (Attribute (icon :: String | r))

icon_ = pure >>> icon

mode
  :: forall r. Poll E.Mode -> Poll (Attribute (mode :: E.Mode | r))

mode = map (E.unMode >>> attributeAtYourOwnRisk "mode")

mode_
  :: forall r. E.Mode -> Poll (Attribute (mode :: E.Mode | r))

mode_ = pure >>> mode

text
  :: forall r. Poll String -> Poll (Attribute (text :: String | r))

text = map (attributeAtYourOwnRisk "text")

text_
  :: forall r. String -> Poll (Attribute (text :: String | r))

text_ = pure >>> text

xtype
  :: forall r. Poll E.ItemType -> Poll (Attribute (xtype :: E.ItemType | r))

xtype = map (E.unItemType >>> attributeAtYourOwnRisk "xtype")

xtype_
  :: forall r. E.ItemType -> Poll (Attribute (xtype :: E.ItemType | r))

xtype_ = pure >>> xtype


type CounterFormatter = Fn2 Number Number String

foreign import routerAnimation
  :: IonBackButton -> RouterAnimation -> Effect Unit

foreign import getRouterAnimation :: IonBackButton -> Effect RouterAnimation