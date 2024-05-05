module Deku.Ionic.Select where

import Prelude

import Control.Plus (empty)
import Data.Maybe (Maybe(..))
import Deku.Attribute (Attribute)
import Deku.Core (Nut, attributeAtYourOwnRisk, callbackWithCaution, elementify)
import Deku.DOM (HTMLElement)
import Deku.DOM.Self (class IsSelf)
import Deku.Ionic.Enums as E
import Effect (Effect)
import FRP.Poll (Poll)
import Foreign (Foreign)
import Safe.Coerce (coerce)
import Type.Proxy (Proxy)
import Web.Event.Internal.Types as Web.Event.Internal.Types
import Yoga.JSON (writeJSON)

data IonSelect

type HTMLIonSelect (r :: Row Type) =
  ( __tag :: Proxy "HTMLIonSelect"
  , cancelText :: String
  , color :: E.Color
  , disabled :: Boolean
  , expandedIcon ::  String
  , fill ::  E.Fill
  , interface :: E.Interface
  , interfaceOptions :: Foreign
  , justify :: E.Justify
  , label ::  String
  , labelPlacement ::  E.LabelPlacement
  , mode ::  E.Mode
  , multiple :: Boolean
  , name :: String
  , okText :: String
  , placeholder ::  String
  , selectedText ::  String
  , shape ::  E.Shape
  , slot :: String
  , toggleIcon ::  String
  , onIonBlur :: IonBlur
  , onIonCancel :: IonCancel
  , onIonChange :: IonChange
  , onIonDismiss :: IonDismiss
  , onIonFocus :: IonFocus
  | HTMLElement r
  )

instance IsSelf IonSelect "HTMLIonSelect"

ionSelect
  :: Array (Poll (Attribute (HTMLIonSelect ()))) -> Array Nut -> Nut
ionSelect = elementify Nothing "ion-select"

ionSelect_
  :: Array Nut
  -> Nut
ionSelect_ = ionSelect empty

-- Attribute and event handler functions

cancelText
  :: forall r. Poll String -> Poll (Attribute (cancelText :: String | r))
cancelText = map $ attributeAtYourOwnRisk "cancel-text"

cancelText_
  :: forall r. String -> Poll (Attribute (cancelText :: String | r))
cancelText_ = pure >>> cancelText

color
  :: forall r. Poll E.Color -> Poll (Attribute (color :: E.Color | r))
color = map $ E.unColor >>> attributeAtYourOwnRisk "color"

color_ :: forall r. E.Color -> Poll (Attribute (color :: E.Color | r))
color_ = pure >>> color

disabled
  :: forall r. Poll Boolean -> Poll (Attribute (disabled :: Boolean | r))
disabled = map $ show >>> attributeAtYourOwnRisk "disabled"

disabled_ :: forall r. Boolean -> Poll (Attribute (disabled :: Boolean | r))
disabled_ = pure >>> disabled

expandedIcon
  :: forall r. Poll String -> Poll (Attribute (expandedIcon :: String | r))
expandedIcon = map $ attributeAtYourOwnRisk "expanded-icon"

expandedIcon_
  :: forall r. String -> Poll (Attribute (expandedIcon :: String | r))
expandedIcon_ = pure >>> expandedIcon

fill
  :: forall r. Poll E.Fill -> Poll (Attribute (fill :: E.Fill | r))
fill = map $ E.unFill >>> attributeAtYourOwnRisk "fill"

fill_
  :: forall r. E.Fill -> Poll (Attribute (fill :: E.Fill | r))
fill_ = pure >>> fill

interface
  :: forall r. Poll E.Interface -> Poll (Attribute (interface :: E.Interface | r))
interface = map $ E.unInterface >>> attributeAtYourOwnRisk "interface"

interface_
  :: forall r. E.Interface -> Poll (Attribute (interface :: E.Interface | r))
interface_ = pure >>> interface

interfaceOptions
  :: forall r. Poll Foreign -> Poll (Attribute (interfaceOptions :: Foreign | r)) -- Define the type for options
interfaceOptions = map $ writeJSON >>> attributeAtYourOwnRisk "interface-options"

interfaceOptions_ :: forall r. Foreign -> Poll (Attribute (interfaceOptions :: Foreign | r))
interfaceOptions_ = pure >>> interfaceOptions

justify
  :: forall r. Poll E.Justify -> Poll (Attribute (justify :: E.Justify | r))
justify = map $ E.unJustify >>> attributeAtYourOwnRisk "justify"

justify_
  :: forall r. E.Justify -> Poll (Attribute (justify :: E.Justify | r))
justify_ = pure >>> justify

label
  :: forall r. Poll String -> Poll (Attribute (label :: String | r))
label = map $ attributeAtYourOwnRisk "label"

label_
  :: forall r. String -> Poll (Attribute (label :: String | r))
label_ = pure >>> label

labelPlacement
  :: forall r. Poll E.LabelPlacement -> Poll (Attribute (labelPlacement :: E.LabelPlacement | r))
labelPlacement = map $ E.unLabelPlacement >>> attributeAtYourOwnRisk "label-placement"

labelPlacement_
  :: forall r. E.LabelPlacement -> Poll (Attribute (labelPlacement :: E.LabelPlacement | r))
labelPlacement_ = pure >>> labelPlacement

mode
  :: forall r. Poll E.Mode -> Poll (Attribute (mode :: E.Mode | r))
mode = map $ E.unMode >>> attributeAtYourOwnRisk "mode"

mode_
  :: forall r. E.Mode -> Poll (Attribute (mode :: E.Mode | r))
mode_ = pure >>> mode

multiple
  :: forall r. Poll Boolean -> Poll (Attribute (multiple :: Boolean | r))
multiple = map (show >>> attributeAtYourOwnRisk "multiple")

multiple_
  :: forall r. Boolean -> Poll (Attribute (multiple :: Boolean | r))
multiple_ = pure >>> multiple

name
  :: forall r. Poll String -> Poll (Attribute (name :: String | r))
name = map $ attributeAtYourOwnRisk "name"

name_
  :: forall r. String -> Poll (Attribute (name :: String | r))
name_ = pure >>> name

okText
  :: forall r. Poll String -> Poll (Attribute (okText :: String | r))
okText = map $ attributeAtYourOwnRisk "ok-text"

okText_
  :: forall r. String -> Poll (Attribute (okText :: String | r))
okText_ = pure >>> okText

placeholder
  :: forall r. Poll String -> Poll (Attribute (placeholder :: String | r))
placeholder = map $ attributeAtYourOwnRisk "placeholder"

placeholder_
  :: forall r. String -> Poll (Attribute (placeholder :: String | r))
placeholder_ = pure >>> placeholder

selectedText
  :: forall r. Poll String -> Poll (Attribute (selectedText :: String | r))
selectedText = map $ attributeAtYourOwnRisk "selected-text"

selectedText_
  :: forall r. String -> Poll (Attribute (selectedText :: String | r))
selectedText_ = pure >>> selectedText

shape
  :: forall r. Poll E.Shape -> Poll (Attribute (shape :: E.Shape | r))
shape = map (E.unShape >>> attributeAtYourOwnRisk "shape")

shape_
  :: forall r. E.Shape -> Poll (Attribute (shape :: E.Shape | r))
shape_ = pure >>> shape

slot :: forall r. Poll String -> Poll (Attribute (slot :: String | r))
slot = map $ attributeAtYourOwnRisk "slot"

slot_ :: forall r. String -> Poll (Attribute (slot :: String | r))
slot_ = pure >>> slot

toggleIcon
  :: forall r. Poll String -> Poll (Attribute (toggleIcon :: String | r))
toggleIcon = map $ attributeAtYourOwnRisk "toggle-icon"

toggleIcon_
  :: forall r. String -> Poll (Attribute (toggleIcon :: String | r))
toggleIcon_ = pure >>> toggleIcon

-- Event handlers using callbackWithCaution for managing effects
newtype IonBlur = IonBlur Web.Event.Internal.Types.Event
newtype IonCancel = IonCancel Web.Event.Internal.Types.Event
newtype IonChange = IonChange Web.Event.Internal.Types.Event
newtype IonDismiss = IonDismiss Web.Event.Internal.Types.Event
newtype IonFocus = IonFocus Web.Event.Internal.Types.Event

onIonChange_
  :: forall r
   . (IonChange -> Effect Unit)
  -> Poll (Attribute (onIonChange :: IonChange | r))
onIonChange_ = pure >>> onIonChange

onIonBlur_
  :: forall r
   . (IonBlur -> Effect Unit)
  -> Poll (Attribute (onIonBlur :: IonBlur | r))
onIonBlur_ = pure >>> onIonBlur

onIonFocus_
  :: forall r
   . (IonFocus -> Effect Unit)
  -> Poll (Attribute (onIonFocus :: IonFocus | r))
onIonFocus_ = pure >>> onIonFocus

onIonCancel_
  :: forall r
   . (IonCancel -> Effect Unit)
  -> Poll (Attribute (onIonCancel :: IonCancel | r))
onIonCancel_ = pure >>> onIonCancel

onIonDismiss_
  :: forall r
   . (IonDismiss -> Effect Unit)
  -> Poll (Attribute (onIonDismiss :: IonDismiss | r))
onIonDismiss_ = pure >>> onIonDismiss

-- Definitions using callbackWithCaution for the event handlers
onIonChange
  :: forall r
   . Poll (IonChange -> Effect Unit)
  -> Poll (Attribute (onIonChange :: IonChange | r))
onIonChange = map ((coerce :: _ -> _ -> _ Unit) >>> map (_ $> true) >>> callbackWithCaution "ionChange")

onIonBlur
  :: forall r
   . Poll (IonBlur -> Effect Unit)
  -> Poll (Attribute (onIonBlur :: IonBlur | r))
onIonBlur = map ((coerce :: _ -> _ -> _ Unit) >>> map (_ $> true) >>> callbackWithCaution "ionBlur")

onIonFocus
  :: forall r
   . Poll (IonFocus -> Effect Unit)
  -> Poll (Attribute (onIonFocus :: IonFocus | r))
onIonFocus = map ((coerce :: _ -> _ -> _ Unit) >>> map (_ $> true) >>> callbackWithCaution "ionFocus")

onIonCancel :: forall r. Poll (IonCancel -> Effect Unit) -> Poll (Attribute (onIonCancel :: IonCancel | r))
onIonCancel = map ((coerce :: _ -> _ -> _ Unit) >>> map (_ $> true) >>> callbackWithCaution "ionCancel")

onIonDismiss :: forall r. Poll (IonDismiss -> Effect Unit) -> Poll (Attribute (onIonDismiss :: IonDismiss | r))
onIonDismiss = map ((coerce :: _ -> _ -> _ Unit) >>> map (_ $> true) >>> callbackWithCaution "ionDismiss")

foreign import value :: IonChange -> Effect String