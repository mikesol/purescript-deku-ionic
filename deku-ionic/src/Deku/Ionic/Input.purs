module Deku.Ionic.Input where

import Prelude hiding (max, min)

import Control.Plus (empty)
import Control.Promise (Promise)
import Data.Maybe (Maybe(..))
import Deku.Attribute (Attribute)
import Deku.Core (Nut, attributeAtYourOwnRisk, callbackWithCaution, elementify)
import Deku.DOM (HTMLElement)
import Deku.DOM.Self (class IsSelf)
import Deku.Ionic.Enums as E
import Effect (Effect)
import FRP.Poll (Poll)
import Safe.Coerce (coerce)
import Type.Proxy (Proxy)
import Web.Event.Internal.Types as Web.Event.Internal.Types
import Web.HTML (HTMLInputElement)

data IonInput

type HTMLIonInput (r :: Row Type) =
  ( __tag :: Proxy "HTMLIonInput"
  , accept :: String
  , autocapitalize :: E.Autocapitalize
  , autocomplete :: E.Autocomplete
  , autocorrect :: E.Autocorrect
  , autofocus :: Boolean
  , clearInput :: Boolean
  , clearOnEdit :: Boolean
  , color :: E.Color
  , debounce :: Number
  , disabled :: Boolean
  , enterkeyhint :: E.Enterkeyhint
  , inputmode :: E.Inputmode
  , max :: Number
  , maxlength :: Number
  , min :: Number
  , minlength :: Number
  , mode :: E.Mode
  , multiple :: Boolean
  , name :: String
  , pattern :: String
  , placeholder :: String
  , readonly :: Boolean
  , required :: Boolean
  , size :: Number
  , spellcheck :: Boolean
  , step :: String
  , xtype :: E.InputType
  , value :: String
  , onIonChange :: IonChange
  , onIonBlur :: IonBlur
  , onIonFocus :: IonFocus
  , onIonInput :: IonInputEvent
  | HTMLElement r
  )

instance IsSelf IonInput "HTMLIonInput"

ionInput
  :: Array (Poll (Attribute (HTMLIonInput ()))) -> Array Nut -> Nut
ionInput = elementify Nothing "ion-input"

ionInput_
  :: Array Nut
  -> Nut
ionInput_ = ionInput empty

-- Attribute handling functions with underscore versions for direct assignment
accept
  :: forall r
   . Poll String
  -> Poll (Attribute (accept :: String | r))
accept = map (attributeAtYourOwnRisk "accept")

accept_ :: forall r. String -> Poll (Attribute (accept :: String | r))
accept_ = pure >>> accept

autocapitalize
  :: forall r
   . Poll E.Autocapitalize
  -> Poll (Attribute (autocapitalize :: E.Autocapitalize | r))
autocapitalize = map (E.unAutocapitalize >>> attributeAtYourOwnRisk "autocapitalize")

autocapitalize_
  :: forall r
   . E.Autocapitalize
  -> Poll (Attribute (autocapitalize :: E.Autocapitalize | r))
autocapitalize_ = pure >>> autocapitalize

autocomplete
  :: forall r
   . Poll E.Autocomplete
  -> Poll (Attribute (autocomplete :: E.Autocomplete | r))
autocomplete = map (E.unAutocomplete >>> attributeAtYourOwnRisk "autocomplete")

autocomplete_
  :: forall r
   . E.Autocomplete
  -> Poll (Attribute (autocomplete :: E.Autocomplete | r))
autocomplete_ = pure >>> autocomplete

autocorrect
  :: forall r
   . Poll E.Autocorrect
  -> Poll (Attribute (autocorrect :: E.Autocorrect | r))
autocorrect = map (E.unAutocorrect >>> attributeAtYourOwnRisk "autocorrect")

autocorrect_
  :: forall r
   . E.Autocorrect
  -> Poll (Attribute (autocorrect :: E.Autocorrect | r))
autocorrect_ = pure >>> autocorrect

autofocus
  :: forall r
   . Poll Boolean
  -> Poll (Attribute (autofocus :: Boolean | r))
autofocus = map (show >>> attributeAtYourOwnRisk "autofocus")

autofocus_ :: forall r. Boolean -> Poll (Attribute (autofocus :: Boolean | r))
autofocus_ = pure >>> autofocus

clearInput
  :: forall r
   . Poll Boolean
  -> Poll (Attribute (clearInput :: Boolean | r))
clearInput = map (show >>> attributeAtYourOwnRisk "clear-input")

clearInput_ :: forall r. Boolean -> Poll (Attribute (clearInput :: Boolean | r))
clearInput_ = pure >>> clearInput

clearOnEdit
  :: forall r
   . Poll Boolean
  -> Poll (Attribute (clearOnEdit :: Boolean | r))
clearOnEdit = map (show >>> attributeAtYourOwnRisk "clear-on-edit")

clearOnEdit_ :: forall r. Boolean -> Poll (Attribute (clearOnEdit :: Boolean | r))
clearOnEdit_ = pure >>> clearOnEdit

color
  :: forall r
   . Poll E.Color
  -> Poll (Attribute (color :: E.Color | r))
color = map (E.unColor >>> attributeAtYourOwnRisk "color")

color_
  :: forall r
   . E.Color
  -> Poll (Attribute (color :: E.Color | r))
color_ = pure >>> color

debounce
  :: forall r
   . Poll Number
  -> Poll (Attribute (debounce :: Number | r))
debounce = map (show >>> attributeAtYourOwnRisk "debounce")

debounce_ :: forall r. Number -> Poll (Attribute (debounce :: Number | r))
debounce_ = pure >>> debounce

disabled
  :: forall r
   . Poll Boolean
  -> Poll (Attribute (disabled :: Boolean | r))
disabled = map (show >>> attributeAtYourOwnRisk "disabled")

disabled_ :: forall r. Boolean -> Poll (Attribute (disabled :: Boolean | r))
disabled_ = pure >>> disabled

enterkeyhint
  :: forall r
   . Poll E.Enterkeyhint
  -> Poll (Attribute (enterkeyhint :: E.Enterkeyhint | r))
enterkeyhint = map (E.unEnterkeyhint >>> attributeAtYourOwnRisk "enterkeyhint")

enterkeyhint_
  :: forall r
   . E.Enterkeyhint
  -> Poll (Attribute (enterkeyhint :: E.Enterkeyhint | r))
enterkeyhint_ = pure >>> enterkeyhint

inputmode
  :: forall r
   . Poll E.Inputmode
  -> Poll (Attribute (inputmode :: E.Inputmode | r))
inputmode = map (E.unInputmode >>> attributeAtYourOwnRisk "inputmode")

inputmode_
  :: forall r
   . E.Inputmode
  -> Poll (Attribute (inputmode :: E.Inputmode | r))
inputmode_ = pure >>> inputmode

max
  :: forall r
   . Poll Number
  -> Poll (Attribute (max :: Number | r))
max = map (show >>> attributeAtYourOwnRisk "max")

max_ :: forall r. Number -> Poll (Attribute (max :: Number | r))
max_ = pure >>> max

maxlength
  :: forall r
   . Poll Number
  -> Poll (Attribute (maxlength :: Number | r))
maxlength = map (show >>> attributeAtYourOwnRisk "maxlength")

maxlength_ :: forall r. Number -> Poll (Attribute (maxlength :: Number | r))
maxlength_ = pure >>> maxlength

min
  :: forall r
   . Poll Number
  -> Poll (Attribute (min :: Number | r))
min = map (show >>> attributeAtYourOwnRisk "min")

min_ :: forall r. Number -> Poll (Attribute (min :: Number | r))
min_ = pure >>> min

minlength
  :: forall r
   . Poll Number
  -> Poll (Attribute (minlength :: Number | r))
minlength = map (show >>> attributeAtYourOwnRisk "minlength")

minlength_ :: forall r. Number -> Poll (Attribute (minlength :: Number | r))
minlength_ = pure >>> minlength

mode
  :: forall r
   . Poll E.Mode
  -> Poll (Attribute (mode :: E.Mode | r))
mode = map (E.unMode >>> attributeAtYourOwnRisk "mode")

mode_
  :: forall r
   . E.Mode
  -> Poll (Attribute (mode :: E.Mode | r))
mode_ = pure >>> mode

multiple
  :: forall r
   . Poll Boolean
  -> Poll (Attribute (multiple :: Boolean | r))
multiple = map (show >>> attributeAtYourOwnRisk "multiple")

multiple_ :: forall r. Boolean -> Poll (Attribute (multiple :: Boolean | r))
multiple_ = pure >>> multiple

name
  :: forall r
   . Poll String
  -> Poll (Attribute (name :: String | r))
name = map (attributeAtYourOwnRisk "name")

name_ :: forall r. String -> Poll (Attribute (name :: String | r))
name_ = pure >>> name

pattern
  :: forall r
   . Poll String
  -> Poll (Attribute (pattern :: String | r))
pattern = map (attributeAtYourOwnRisk "pattern")

pattern_ :: forall r. String -> Poll (Attribute (pattern :: String | r))
pattern_ = pure >>> pattern

placeholder
  :: forall r
   . Poll String
  -> Poll (Attribute (placeholder :: String | r))
placeholder = map (attributeAtYourOwnRisk "placeholder")

placeholder_ :: forall r. String -> Poll (Attribute (placeholder :: String | r))
placeholder_ = pure >>> placeholder

readonly
  :: forall r
   . Poll Boolean
  -> Poll (Attribute (readonly :: Boolean | r))
readonly = map (show >>> attributeAtYourOwnRisk "readonly")

readonly_ :: forall r. Boolean -> Poll (Attribute (readonly :: Boolean | r))
readonly_ = pure >>> readonly

required
  :: forall r
   . Poll Boolean
  -> Poll (Attribute (required :: Boolean | r))
required = map (show >>> attributeAtYourOwnRisk "required")

required_ :: forall r. Boolean -> Poll (Attribute (required :: Boolean | r))
required_ = pure >>> required

size
  :: forall r
   . Poll Number
  -> Poll (Attribute (size :: Number | r))
size = map (show >>> attributeAtYourOwnRisk "size")

size_ :: forall r. Number -> Poll (Attribute (size :: Number | r))
size_ = pure >>> size

spellcheck
  :: forall r
   . Poll Boolean
  -> Poll (Attribute (spellcheck :: Boolean | r))
spellcheck = map (show >>> attributeAtYourOwnRisk "spellcheck")

spellcheck_ :: forall r. Boolean -> Poll (Attribute (spellcheck :: Boolean | r))
spellcheck_ = pure >>> spellcheck

step
  :: forall r
   . Poll String
  -> Poll (Attribute (step :: String | r))
step = map (attributeAtYourOwnRisk "step")

step_ :: forall r. String -> Poll (Attribute (step :: String | r))
step_ = pure >>> step

xtype
  :: forall r
   . Poll E.InputType
  -> Poll (Attribute (xtype :: E.InputType | r))
xtype = map (E.unInputType >>> attributeAtYourOwnRisk "xtype")

xtype_
  :: forall r
   . E.InputType
  -> Poll (Attribute (xtype :: E.InputType | r))
xtype_ = pure >>> xtype

value
  :: forall r
   . Poll String
  -> Poll (Attribute (value :: String | r))
value = map (attributeAtYourOwnRisk "value")

value_ :: forall r. String -> Poll (Attribute (value :: String | r))
value_ = pure >>> value

-- Direct assignment functions for event listeners in IonInput

newtype IonChange = IonChange Web.Event.Internal.Types.Event
newtype IonBlur = IonBlur Web.Event.Internal.Types.Event
newtype IonFocus = IonFocus Web.Event.Internal.Types.Event
newtype IonInputEvent = IonInputEvent Web.Event.Internal.Types.Event

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

onIonInput_
  :: forall r
   . (IonInputEvent -> Effect Unit)
  -> Poll (Attribute (onIonInput :: IonInputEvent | r))
onIonInput_ = pure >>> onIonInput

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

onIonInput
  :: forall r
   . Poll (IonInputEvent -> Effect Unit)
  -> Poll (Attribute (onIonInput :: IonInputEvent | r))
onIonInput = map ((coerce :: _ -> _ -> _ Unit) >>> map (_ $> true) >>> callbackWithCaution "ionInput")

foreign import getInputElement :: IonInput -> Effect (Promise HTMLInputElement)
foreign import setFocus :: IonInput -> Effect (Promise Unit)