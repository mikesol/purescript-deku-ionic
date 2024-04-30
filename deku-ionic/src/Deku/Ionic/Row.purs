module Deku.Ionic.Row where

import Control.Plus (empty)
import Data.Maybe (Maybe(..))
import Deku.Attribute (Attribute)
import Deku.Core (Nut, elementify)
import Deku.DOM (HTMLElement)
import FRP.Poll (Poll)
import Type.Proxy (Proxy)

type HTMLIonRow (r :: Row Type) =
  ( __tag :: Proxy "HTMLIonRow"
  | HTMLElement r
  )

ionRow
  :: Array (Poll (Attribute (HTMLIonRow ()))) -> Array Nut -> Nut
ionRow = elementify Nothing "ion-row"

ionRow_
  :: Array Nut
  -> Nut
ionRow_ = ionRow empty
