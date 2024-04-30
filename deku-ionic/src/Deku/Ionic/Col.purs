module Deku.Ionic.Col where

import Prelude hiding (max, min)

import Control.Plus (empty)
import Data.Maybe (Maybe(..))
import Deku.Attribute (Attribute)
import Deku.Core (Nut, attributeAtYourOwnRisk, elementify)
import Deku.DOM (HTMLElement)
import Deku.DOM.Self (class IsSelf)
import FRP.Poll (Poll)
import Type.Proxy (Proxy)

data IonCol

type HTMLIonCol (r :: Row Type) =
  ( __tag :: Proxy "HTMLIonCol"
  , offset :: String
  , offsetLg :: String
  , offsetMd :: String
  , offsetSm :: String
  , offsetXs :: String
  , offsetXl :: String
  , pull :: String
  , pullLg :: String
  , pullMd :: String
  , pullSm :: String
  , pullXs :: String
  , pullXl :: String
  , size :: String
  , sizeLg :: String
  , sizeMd :: String
  , sizeSm :: String
  , sizeXs :: String
  , sizeXl :: String
  , push :: String
  , pushLg :: String
  , pushMd :: String
  , pushSm :: String
  , pushXs :: String
  , pushXl :: String
  | HTMLElement r
  )

instance IsSelf IonCol "HTMLIonCol"

ionCol
  :: Array (Poll (Attribute (HTMLIonCol ()))) -> Array Nut -> Nut
ionCol = elementify Nothing "ion-col"

ionCol_
  :: Array Nut -> Nut
ionCol_ = ionCol empty

-- Define attribute setters for each attribute and their underscore versions
-- Following functions are representative; replicate for each attribute as needed

-- Offset attributes
offset
  :: forall r. Poll String -> Poll (Attribute (offset :: String | r))
offset = map (attributeAtYourOwnRisk "offset")

offset_
  :: forall r. String -> Poll (Attribute (offset :: String | r))
offset_ = pure >>> offset

-- Repeat for offsetLg, offsetMd, offsetSm, offsetXs, offsetXl, etc.
-- Example for offsetLg
offsetLg
  :: forall r. Poll String -> Poll (Attribute (offsetLg :: String | r))
offsetLg = map (attributeAtYourOwnRisk "offset-lg")

offsetLg_
  :: forall r. String -> Poll (Attribute (offsetLg :: String | r))
offsetLg_ = pure >>> offsetLg

offsetMd
  :: forall r. Poll String -> Poll (Attribute (offsetMd :: String | r))
offsetMd = map (attributeAtYourOwnRisk "offset-md")

offsetMd_
  :: forall r. String -> Poll (Attribute (offsetMd :: String | r))
offsetMd_ = pure >>> offsetMd

offsetSm
  :: forall r. Poll String -> Poll (Attribute (offsetSm :: String | r))
offsetSm = map (attributeAtYourOwnRisk "offset-sm")

offsetSm_
  :: forall r. String -> Poll (Attribute (offsetSm :: String | r))
offsetSm_ = pure >>> offsetSm

offsetXs
  :: forall r. Poll String -> Poll (Attribute (offsetXs :: String | r))
offsetXs = map (attributeAtYourOwnRisk "offset-xs")

offsetXs_
  :: forall r. String -> Poll (Attribute (offsetXs :: String | r))
offsetXs_ = pure >>> offsetXs

offsetXl
  :: forall r. Poll String -> Poll (Attribute (offsetXl :: String | r))
offsetXl = map (attributeAtYourOwnRisk "offset-xl")

offsetXl_
  :: forall r. String -> Poll (Attribute (offsetXl :: String | r))
offsetXl_ = pure >>> offsetXl

-- Pull attributes

pull
  :: forall r. Poll String -> Poll (Attribute (pull :: String | r))
pull = map (attributeAtYourOwnRisk "pull")

pull_
  :: forall r. String -> Poll (Attribute (pull :: String | r))
pull_ = pure >>> pull

pullLg
  :: forall r. Poll String -> Poll (Attribute (pullLg :: String | r))

pullLg = map (attributeAtYourOwnRisk "pull-lg")

pullLg_
  :: forall r. String -> Poll (Attribute (pullLg :: String | r))
pullLg_ = pure >>> pullLg

pullMd
  :: forall r. Poll String -> Poll (Attribute (pullMd :: String | r))
pullMd = map (attributeAtYourOwnRisk "pull-md")

pullMd_
  :: forall r. String -> Poll (Attribute (pullMd :: String | r))
pullMd_ = pure >>> pullMd

pullSm
  :: forall r. Poll String -> Poll (Attribute (pullSm :: String | r))
pullSm = map (attributeAtYourOwnRisk "pull-sm")

pullSm_
  :: forall r. String -> Poll (Attribute (pullSm :: String | r))
pullSm_ = pure >>> pullSm

pullXs
  :: forall r. Poll String -> Poll (Attribute (pullXs :: String | r))
pullXs = map (attributeAtYourOwnRisk "pull-xs")

pullXs_
  :: forall r. String -> Poll (Attribute (pullXs :: String | r))
pullXs_ = pure >>> pullXs

pullXl
  :: forall r. Poll String -> Poll (Attribute (pullXl :: String | r))
pullXl = map (attributeAtYourOwnRisk "pull-xl")

pullXl_
  :: forall r. String -> Poll (Attribute (pullXl :: String | r))
pullXl_ = pure >>> pullXl

-- Size attributes

size
  :: forall r. Poll String -> Poll (Attribute (size :: String | r))
size = map (attributeAtYourOwnRisk "size")

size_
  :: forall r. String -> Poll (Attribute (size :: String | r))
size_ = pure >>> size

sizeLg
  :: forall r. Poll String -> Poll (Attribute (sizeLg :: String | r))

sizeLg = map (attributeAtYourOwnRisk "size-lg")

sizeLg_
  :: forall r. String -> Poll (Attribute (sizeLg :: String | r))
sizeLg_ = pure >>> sizeLg

sizeMd
  :: forall r. Poll String -> Poll (Attribute (sizeMd :: String | r))
sizeMd = map (attributeAtYourOwnRisk "size-md")

sizeMd_
  :: forall r. String -> Poll (Attribute (sizeMd :: String | r))
sizeMd_ = pure >>> sizeMd

sizeSm
  :: forall r. Poll String -> Poll (Attribute (sizeSm :: String | r))
sizeSm = map (attributeAtYourOwnRisk "size-sm")

sizeSm_
  :: forall r. String -> Poll (Attribute (sizeSm :: String | r))
sizeSm_ = pure >>> sizeSm

sizeXs
  :: forall r. Poll String -> Poll (Attribute (sizeXs :: String | r))
sizeXs = map (attributeAtYourOwnRisk "size-xs")

sizeXs_
  :: forall r. String -> Poll (Attribute (sizeXs :: String | r))
sizeXs_ = pure >>> sizeXs

sizeXl
  :: forall r. Poll String -> Poll (Attribute (sizeXl :: String | r))
sizeXl = map (attributeAtYourOwnRisk "size-xl")

sizeXl_
  :: forall r. String -> Poll (Attribute (sizeXl :: String | r))
sizeXl_ = pure >>> sizeXl

-- Push attributes

push
  :: forall r. Poll String -> Poll (Attribute (push :: String | r))
push = map (attributeAtYourOwnRisk "push")

push_
  :: forall r. String -> Poll (Attribute (push :: String | r))
push_ = pure >>> push

pushLg
  :: forall r. Poll String -> Poll (Attribute (pushLg :: String | r))

pushLg = map (attributeAtYourOwnRisk "push-lg")

pushLg_
  :: forall r. String -> Poll (Attribute (pushLg :: String | r))
pushLg_ = pure >>> pushLg

pushMd
  :: forall r. Poll String -> Poll (Attribute (pushMd :: String | r))
pushMd = map (attributeAtYourOwnRisk "push-md")

pushMd_
  :: forall r. String -> Poll (Attribute (pushMd :: String | r))
pushMd_ = pure >>> pushMd

pushSm
  :: forall r. Poll String -> Poll (Attribute (pushSm :: String | r))
pushSm = map (attributeAtYourOwnRisk "push-sm")

pushSm_
  :: forall r. String -> Poll (Attribute (pushSm :: String | r))
pushSm_ = pure >>> pushSm

pushXs
  :: forall r. Poll String -> Poll (Attribute (pushXs :: String | r))
pushXs = map (attributeAtYourOwnRisk "push-xs")

pushXs_
  :: forall r. String -> Poll (Attribute (pushXs :: String | r))
pushXs_ = pure >>> pushXs

pushXl
  :: forall r. Poll String -> Poll (Attribute (pushXl :: String | r))
pushXl = map (attributeAtYourOwnRisk "push-xl")

pushXl_
  :: forall r. String -> Poll (Attribute (pushXl :: String | r))

pushXl_ = pure >>> pushXl
