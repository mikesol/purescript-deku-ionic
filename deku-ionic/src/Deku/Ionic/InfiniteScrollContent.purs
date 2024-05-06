module Deku.Ionic.InfiniteScrollContent where

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

data IonInfiniteScrollContent

type HTMLIonInfiniteScrollContent (r :: Row Type) =
  ( __tag :: Proxy "HTMLIonInfiniteScrollContent"
  , loadingSpinner :: E.LoadingSpinner
  , loadingText :: String
  | HTMLElement r
  )

instance IsSelf IonInfiniteScrollContent "HTMLIonInfiniteScrollContent"

ionInfiniteScrollContent
  :: Array (Poll (Attribute (HTMLIonInfiniteScrollContent ()))) -> Array Nut -> Nut
ionInfiniteScrollContent = elementify Nothing "ion-infinite-scroll-content"

ionInfiniteScrollContent_
  :: Array Nut -> Nut
ionInfiniteScrollContent_ = ionInfiniteScrollContent empty

loadingSpinner :: forall r. Poll E.LoadingSpinner -> Poll (Attribute (loadingSpinner :: E.LoadingSpinner | r))
loadingSpinner = map (E.unLoadingSpinner >>> attributeAtYourOwnRisk "loading-spinner")

loadingSpinner_ :: forall r. E.LoadingSpinner -> Poll (Attribute (loadingSpinner :: E.LoadingSpinner | r))
loadingSpinner_ = pure >>> loadingSpinner

loadingText :: forall r. Poll String -> Poll (Attribute (loadingText :: String | r))
loadingText = map (attributeAtYourOwnRisk "loading-text")

loadingText_ :: forall r. String -> Poll (Attribute (loadingText :: String | r))
loadingText_ = pure >>> loadingText