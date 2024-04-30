module Deku.Ionic.Title where

import Prelude

import Control.Plus (empty)
import Data.Maybe (Maybe(..))
import Deku.Attribute (Attribute)
import Deku.Core (Nut, attributeAtYourOwnRisk, elementify)
import Deku.DOM (HTMLElement)
import Deku.Ionic.Enums as E
import FRP.Poll (Poll)
import Type.Proxy (Proxy)

type HTMLIonTitle (r :: Row Type) =
  ( __tag :: Proxy "HTMLIonTitle"
  , color :: String
  , size :: String
  | HTMLElement r
  )

ionTitle
  :: Array (Poll (Attribute (HTMLIonTitle ()))) -> Array Nut -> Nut
ionTitle = elementify Nothing "ion-title"

ionTitle_
  :: Array Nut
  -> Nut
ionTitle_ = ionTitle empty

color
  :: forall r
   . Poll E.Color
  -> Poll (Attribute (color :: String | r))
color = map (E.unColor >>> attributeAtYourOwnRisk "color")

color_
  :: forall r
   . E.Color
  -> Poll (Attribute (color :: String | r))
color_ = pure >>> color

size
  :: forall r
   . Poll E.TitleSize
  -> Poll (Attribute (size :: String | r))
size = map (E.unTitleSize >>> attributeAtYourOwnRisk "size")

size_
  :: forall r
   . E.TitleSize
  -> Poll (Attribute (size :: String | r))
size_ = pure >>> size
