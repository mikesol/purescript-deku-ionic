module Deku.Ionic.InfiniteScroll where

import Prelude hiding (max, min)

import Control.Plus (empty)
import Control.Promise (Promise)
import Data.Maybe (Maybe(..))
import Deku.Attribute (Attribute)
import Deku.Core (Nut, attributeAtYourOwnRisk, elementify, callbackWithCaution)
import Deku.DOM (HTMLElement)
import Deku.DOM.Self (class IsSelf)
import Deku.Ionic.Enums as E
import Effect (Effect)
import FRP.Poll (Poll)
import Safe.Coerce (coerce)
import Type.Proxy (Proxy)
import Web.Event.Internal.Types as Web.Event.Internal.Types

data IonInfiniteScroll

type HTMLIonInfiniteScroll (r :: Row Type) =
  ( __tag :: Proxy "HTMLIonInfiniteScroll"
  , disabled :: Boolean
  , position :: E.Position
  , threshold :: String
  , onIonInfinite :: IonInfinite
  | HTMLElement r
  )

instance IsSelf IonInfiniteScroll "HTMLIonInfiniteScroll"

ionInfiniteScroll
  :: Array (Poll (Attribute (HTMLIonInfiniteScroll ()))) -> Array Nut -> Nut
ionInfiniteScroll = elementify Nothing "ion-infinite-scroll"

ionInfiniteScroll_
  :: Array Nut -> Nut
ionInfiniteScroll_ = ionInfiniteScroll empty

disabled
  :: forall r. Poll Boolean -> Poll (Attribute (disabled :: Boolean | r))
disabled = map (show >>> attributeAtYourOwnRisk "disabled")

disabled_
  :: forall r. Boolean -> Poll (Attribute (disabled :: Boolean | r))
disabled_ = pure >>> disabled

position
  :: forall r. Poll E.Position -> Poll (Attribute (position :: E.Position | r))
position = map (E.unPosition >>> attributeAtYourOwnRisk "position")

position_
  :: forall r. E.Position -> Poll (Attribute (position :: E.Position | r))
position_ = pure >>> position


threshold
  :: forall r. Poll String -> Poll (Attribute (threshold :: String | r))
threshold = map (attributeAtYourOwnRisk "threshold")

threshold_
  :: forall r. String -> Poll (Attribute (threshold :: String | r))
threshold_ = pure >>> threshold


newtype IonInfinite = IonInfinite Web.Event.Internal.Types.Event

-- Callback functions for focus and blur events using callbackWithCaution
onIonInfinite
  :: forall r. Poll (IonInfinite -> Effect Unit) -> Poll (Attribute (onIonInfinite :: IonInfinite | r))
onIonInfinite = map ((coerce :: _ -> _ -> _ Unit) >>> map (_ $> true) >>> callbackWithCaution "ionInfinite")

onIonInfinite_
  :: forall r. (IonInfinite -> Effect Unit) -> Poll (Attribute (onIonInfinite :: IonInfinite | r))
onIonInfinite_ = pure >>> onIonInfinite

foreign import complete :: IonInfinite -> Effect (Promise Unit)
