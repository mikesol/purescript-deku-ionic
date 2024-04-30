module Deku.Ionic.Modal where

import Prelude

import Control.Plus (empty)
import Control.Promise (Promise)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Deku.Attribute (Attribute)
import Deku.Core (Nut, elementify, attributeAtYourOwnRisk, callbackWithCaution)
import Deku.DOM (HTMLElement)
import Deku.DOM.Self (class IsSelf)
import Deku.Ionic.Enums as E
import Deku.Ionic.Unsafe (EnterAnimation, LeaveAnimation)
import Effect (Effect)
import FRP.Poll (Poll)
import Foreign (Foreign)
import Foreign.Object (Object)
import Safe.Coerce (coerce)
import Type.Proxy (Proxy)
import Web.DOM as Web.DOM
import Web.Event.Internal.Types as Web.Event.Internal.Types

data IonModal

type HTMLIonModal (r :: Row Type) =
  ( __tag :: Proxy "HTMLIonModal"
  , animated :: Boolean
  , backdropBreakpoint :: Number
  , backdropDismiss :: Boolean
  , canDismiss :: Boolean
  , handle :: Boolean
  , handleBehavior :: E.HandleBehavior
  , initialBreakpoint :: Number
  , isOpen :: Boolean
  , keepContentsMounted :: Boolean
  , keyboardClose :: Boolean
  , mode :: E.Mode
  , showBackdrop :: Boolean
  , trigger :: String
  , ionBreakpointDidChange :: IonBreakpointDidChange
  , ionModalDidDismiss :: IonModalDidDismiss
  , ionModalDidPresent :: IonModalDidPresent
  , ionModalWillDismiss :: IonModalWillDismiss
  , ionModalWillPresent :: IonModalWillPresent
  | HTMLElement r
  )

newtype IonBreakpointDidChange = IonBreakpointDidChange Web.Event.Internal.Types.Event
newtype IonModalDidDismiss = IonModalDidDismiss Web.Event.Internal.Types.Event
newtype IonModalDidPresent = IonModalDidPresent Web.Event.Internal.Types.Event
newtype IonModalWillDismiss = IonModalWillDismiss Web.Event.Internal.Types.Event
newtype IonModalWillPresent = IonModalWillPresent Web.Event.Internal.Types.Event

instance IsSelf IonModal "HTMLIonModal"

ionModal
  :: Array (Poll (Attribute (HTMLIonModal ()))) -> Array Nut -> Nut
ionModal = elementify Nothing "ion-modal"

ionModal_
  :: Array Nut
  -> Nut
ionModal_ = ionModal empty

-- Attribute handling functions for IonModal

animated
  :: forall r. Poll Boolean
  -> Poll (Attribute (animated :: Boolean | r))
animated = map (show >>> attributeAtYourOwnRisk "animated")

backdropBreakpoint
  :: forall r. Poll Number
  -> Poll (Attribute (backdropBreakpoint :: Number | r))
backdropBreakpoint = map (show >>> attributeAtYourOwnRisk "backdrop-breakpoint")

backdropDismiss
  :: forall r. Poll Boolean
  -> Poll (Attribute (backdropDismiss :: Boolean | r))
backdropDismiss = map (show >>> attributeAtYourOwnRisk "backdrop-dismiss")

canDismiss
  :: forall r. Poll Boolean
  -> Poll (Attribute (canDismiss :: Boolean | r))
canDismiss = map (show >>> attributeAtYourOwnRisk "can-dismiss")

handle
  :: forall r. Poll Boolean
  -> Poll (Attribute (handle :: Boolean | r))
handle = map (show >>> attributeAtYourOwnRisk "handle")

handleBehavior
  :: forall r. Poll E.HandleBehavior
  -> Poll (Attribute (handleBehavior :: E.HandleBehavior | r))
handleBehavior = map (E.unHandleBehavior >>> attributeAtYourOwnRisk "handle-behavior")

initialBreakpoint
  :: forall r. Poll Number
  -> Poll (Attribute (initialBreakpoint :: Number | r))
initialBreakpoint = map (show >>> attributeAtYourOwnRisk "initial-breakpoint")

isOpen
  :: forall r. Poll Boolean
  -> Poll (Attribute (isOpen :: Boolean | r))
isOpen = map (show >>> attributeAtYourOwnRisk "is-open")

keepContentsMounted
  :: forall r. Poll Boolean
  -> Poll (Attribute (keepContentsMounted :: Boolean | r))
keepContentsMounted = map (show >>> attributeAtYourOwnRisk "keep-contents-mounted")

keyboardClose
  :: forall r. Poll Boolean
  -> Poll (Attribute (keyboardClose :: Boolean | r))
keyboardClose = map (show >>> attributeAtYourOwnRisk "keyboard-close")

mode
  :: forall r. Poll E.Mode
  -> Poll (Attribute (mode :: E.Mode | r))
mode = map (E.unMode >>> attributeAtYourOwnRisk "mode")

showBackdrop
  :: forall r. Poll Boolean
  -> Poll (Attribute (showBackdrop :: Boolean | r))
showBackdrop = map (show >>> attributeAtYourOwnRisk "show-backdrop")

trigger
  :: forall r. Poll String
  -> Poll (Attribute (trigger :: String | r))
trigger = map (attributeAtYourOwnRisk "trigger")


-- Event handling functions for IonModal

ionBreakpointDidChange
  :: forall r. Poll (IonBreakpointDidChange -> Effect Unit)
  -> Poll (Attribute (ionBreakpointDidChange :: IonBreakpointDidChange | r))
ionBreakpointDidChange = map ((coerce :: _ -> _ -> _ Unit) >>> map (_ $> true) >>> callbackWithCaution "ionBreakpointDidChange")

ionModalDidDismiss
  :: forall r. Poll (IonModalDidDismiss -> Effect Unit)
  -> Poll (Attribute (ionModalDidDismiss :: IonModalDidDismiss | r))
ionModalDidDismiss = map ((coerce :: _ -> _ -> _ Unit) >>> map (_ $> true) >>> callbackWithCaution "ionModalDidDismiss")

ionModalDidPresent
  :: forall r. Poll (IonModalDidPresent -> Effect Unit)
  -> Poll (Attribute (ionModalDidPresent :: IonModalDidPresent | r))
ionModalDidPresent = map ((coerce :: _ -> _ -> _ Unit) >>> map (_ $> true) >>> callbackWithCaution "ionModalDidPresent")

ionModalWillDismiss
  :: forall r. Poll (IonModalWillDismiss -> Effect Unit)
  -> Poll (Attribute (ionModalWillDismiss :: IonModalWillDismiss | r))
ionModalWillDismiss = map ((coerce :: _ -> _ -> _ Unit) >>> map (_ $> true) >>> callbackWithCaution "ionModalWillDismiss")

ionModalWillPresent
  :: forall r. Poll (IonModalWillPresent -> Effect Unit)
  -> Poll (Attribute (ionModalWillPresent :: IonModalWillPresent | r))
ionModalWillPresent = map ((coerce :: _ -> _ -> _ Unit) >>> map (_ $> true) >>> callbackWithCaution "ionModalWillPresent")


-- Direct assignment functions for attributes in IonModal

animated_
  :: forall r
   . Boolean
  -> Poll (Attribute (animated :: Boolean | r))
animated_ = pure >>> animated

backdropBreakpoint_
  :: forall r
   . Number
  -> Poll (Attribute (backdropBreakpoint :: Number | r))
backdropBreakpoint_ = pure >>> backdropBreakpoint

backdropDismiss_
  :: forall r
   . Boolean
  -> Poll (Attribute (backdropDismiss :: Boolean | r))
backdropDismiss_ = pure >>> backdropDismiss

canDismiss_
  :: forall r
   . Boolean
  -> Poll (Attribute (canDismiss :: Boolean | r))
canDismiss_ = pure >>> canDismiss

handle_
  :: forall r
   . Boolean
  -> Poll (Attribute (handle :: Boolean | r))
handle_ = pure >>> handle

handleBehavior_
  :: forall r
   . E.HandleBehavior
  -> Poll (Attribute (handleBehavior :: E.HandleBehavior | r))
handleBehavior_ = pure >>> handleBehavior

initialBreakpoint_
  :: forall r
   . Number
  -> Poll (Attribute (initialBreakpoint :: Number | r))
initialBreakpoint_ = pure >>> initialBreakpoint

isOpen_
  :: forall r
   . Boolean
  -> Poll (Attribute (isOpen :: Boolean | r))
isOpen_ = pure >>> isOpen

keepContentsMounted_
  :: forall r
   . Boolean
  -> Poll (Attribute (keepContentsMounted :: Boolean | r))
keepContentsMounted_ = pure >>> keepContentsMounted

keyboardClose_
  :: forall r
   . Boolean
  -> Poll (Attribute (keyboardClose :: Boolean | r))
keyboardClose_ = pure >>> keyboardClose

mode_
  :: forall r
   . E.Mode
  -> Poll (Attribute (mode :: E.Mode | r))
mode_ = pure >>> mode

showBackdrop_
  :: forall r
   . Boolean
  -> Poll (Attribute (showBackdrop :: Boolean | r))
showBackdrop_ = pure >>> showBackdrop

trigger_
  :: forall r
   . String
  -> Poll (Attribute (trigger :: String | r))
trigger_ = pure >>> trigger

-- Direct assignment functions for event listeners in IonModal

ionBreakpointDidChange_
  :: forall r
   . (IonBreakpointDidChange -> Effect Unit)
  -> Poll (Attribute (ionBreakpointDidChange :: IonBreakpointDidChange | r))
ionBreakpointDidChange_ = pure >>> ionBreakpointDidChange

ionModalDidDismiss_
  :: forall r
   . (IonModalDidDismiss -> Effect Unit)
  -> Poll (Attribute (ionModalDidDismiss :: IonModalDidDismiss | r))
ionModalDidDismiss_ = pure >>> ionModalDidDismiss

ionModalDidPresent_
  :: forall r
   . (IonModalDidPresent -> Effect Unit)
  -> Poll (Attribute (ionModalDidPresent :: IonModalDidPresent | r))
ionModalDidPresent_ = pure >>> ionModalDidPresent

ionModalWillDismiss_
  :: forall r
   . (IonModalWillDismiss -> Effect Unit)
  -> Poll (Attribute (ionModalWillDismiss :: IonModalWillDismiss | r))
ionModalWillDismiss_ = pure >>> ionModalWillDismiss

ionModalWillPresent_
  :: forall r
   . (IonModalWillPresent -> Effect Unit)
  -> Poll (Attribute (ionModalWillPresent :: IonModalWillPresent | r))
ionModalWillPresent_ = pure >>> ionModalWillPresent

foreign import breakpoints :: IonModal -> Array Number -> Effect Unit
foreign import getBreakpoints :: IonModal -> Effect (Nullable (Array Number))
foreign import enterAnimation :: IonModal -> EnterAnimation -> Effect Unit
foreign import getEnterAnimation :: IonModal -> Effect EnterAnimation
foreign import htmlAttributes :: IonModal -> Object String -> Effect Unit
foreign import getHtmlAttributes :: IonModal -> Effect (Object String)
foreign import leaveAnimation :: IonModal -> LeaveAnimation -> Effect Unit
foreign import getLeaveAnimation :: IonModal -> Effect LeaveAnimation
foreign import presentingElement :: IonModal -> Web.DOM.Element -> Effect Unit
foreign import getPresentingElement :: IonModal -> Effect (Web.DOM.Element)
foreign import dismiss
  :: IonModal -> Foreign -> String -> Effect (Promise Boolean)

foreign import getCurrentBreakpoint
  :: IonModal -> Effect (Promise (Nullable Number))

foreign import onDidDismiss :: IonModal -> Effect (Promise Unit)
foreign import onWillDismiss :: IonModal -> Effect (Promise Unit)
foreign import present :: IonModal -> Effect (Promise Unit)
foreign import setCurrentBreakpoint
  :: IonModal -> Number -> Effect (Promise Unit)