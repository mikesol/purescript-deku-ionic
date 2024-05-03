module Deku.Ionic.Router where

import Prelude

import Data.Array (replicate)
import Data.Char (fromCharCode)
import Data.FormURLEncoded (encode, fromArray)
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Nullable (Nullable)
import Data.String.CodeUnits (fromCharArray)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Deku.Attribute (Attribute)
import Deku.Core (Nut, attributeAtYourOwnRisk, callbackWithCaution, elementify)
import Deku.DOM (HTMLElement)
import Deku.DOM.Self (class IsSelf)
import Deku.Ionic.Enums as E
import Deku.Ionic.Route (unsafeCustomComponent)
import Deku.Ionic.RouterLink (HTMLIonRouterLink)
import Deku.Ionic.Unsafe as U
import Effect (Effect)
import Effect.Random (randomInt)
import FRP.Poll (Poll)
import Partial.Unsafe (unsafePartial)
import Prim.Row as Row
import Prim.RowList (class RowToList)
import Prim.RowList as RL
import Record (insert)
import Record as Record
import Safe.Coerce (coerce)
import Type.Equality as TE
import Type.Proxy (Proxy(..))
import Web.Event.Internal.Types as Web.Event.Internal.Types
import Yoga.JSON (readJSON_, writeJSON)
import Yoga.JSON as Yoga

data IonRouter

type HTMLIonRouter (r :: Row Type) =
  ( __tag :: Proxy "HTMLIonRouter"
  , root :: String
  , useHash :: Boolean
  , ionRouteDidChange :: IonRouteDidChange
  , ionRouteWillChange :: IonRouteWillChange
  | HTMLElement r
  )

newtype IonRouteDidChange = IonRouteDidChange Web.Event.Internal.Types.Event
newtype IonRouteWillChange = IonRouteWillChange Web.Event.Internal.Types.Event

instance IsSelf IonRouter "HTMLIonRouter"

root
  :: forall r
   . Poll String
  -> Poll (Attribute (root :: String | r))
root = map (attributeAtYourOwnRisk "root")

useHash
  :: forall r
   . Poll Boolean
  -> Poll (Attribute (useHash :: Boolean | r))
useHash = map (show >>> attributeAtYourOwnRisk "use-hash")

ionRouteDidChange
  :: forall r
   . Poll (IonRouteDidChange -> Effect Unit)
  -> Poll (Attribute (ionRouteDidChange :: IonRouteDidChange | r))
ionRouteDidChange = map ((coerce :: _ -> _ -> _ Unit) >>> map (_ $> true) >>> callbackWithCaution "ionRouteDidChange")

ionRouteWillChange
  :: forall r
   . Poll (IonRouteWillChange -> Effect Unit)
  -> Poll (Attribute (ionRouteWillChange :: IonRouteWillChange | r))
ionRouteWillChange = map ((coerce :: _ -> _ -> _ Unit) >>> map (_ $> true) >>> callbackWithCaution "ionRouteWillChange")

-- Direct assignment functions for attributes in IonRouter

root_
  :: forall r
   . String
  -> Poll (Attribute (root :: String | r))
root_ = pure >>> root

useHash_
  :: forall r
   . Boolean
  -> Poll (Attribute (useHash :: Boolean | r))
useHash_ = pure >>> useHash

-- Direct assignment functions for event listeners in IonRouter

ionRouteDidChange_
  :: forall r
   . (IonRouteDidChange -> Effect Unit)
  -> Poll (Attribute (ionRouteDidChange :: IonRouteDidChange | r))
ionRouteDidChange_ = pure >>> ionRouteDidChange

ionRouteWillChange_
  :: forall r
   . (IonRouteWillChange -> Effect Unit)
  -> Poll (Attribute (ionRouteWillChange :: IonRouteWillChange | r))
ionRouteWillChange_ = pure >>> ionRouteWillChange

foreign import back :: IonRouter -> Effect Unit
foreign import push
  :: IonRouter
  -> String
  -> Nullable E.RouterDirection
  -> Nullable U.AnimationBuilder
  -> Effect Unit

---

-- Function to generate a random lowercase letter
randomLetter :: Effect Char
randomLetter = do
  index <- randomInt 97 122
  pure $ fromMaybe 'z' $ fromCharCode index

-- Function to generate a part of the pattern (either before or after the dash)
randomLetters :: Int -> Effect String
randomLetters n = do
  letters <- sequence $ replicate n randomLetter
  pure $ fromCharArray letters

-- Function to generate the full pattern "xxxxx-xxxxx"
generateEltName :: Effect String
generateEltName = do
  firstPart <- randomLetters 5
  secondPart <- randomLetters 5
  pure $ firstPart <> "-" <> secondPart

---
newtype IonRouteDefinition links args = IonRouteDefinition (links -> args -> Nut)

class ConstructLinkDefs :: forall k1 k2. k1 -> k2 -> Constraint
class ConstructLinkDefs i o | i -> o

class ConstructLinkDefs' :: forall k1 k2 k3. k1 -> k2 -> k3 -> Constraint
class ConstructLinkDefs' i rl o | i rl -> o

instance (RL.RowToList i rl, ConstructLinkDefs' i rl o) => ConstructLinkDefs i o

instance ConstructLinkDefs' i RL.Nil ()

instance
  ( ConstructLinkDefs' i rest o'
  , Row.Cons key (Poll args -> Array (Poll (Attribute (HTMLIonRouterLink ()))) -> Array Nut -> Nut) o' o
  ) =>
  ConstructLinkDefs' i (RL.Cons key (IonRouteDefinition links args) rest) o

instance
  ( ConstructLinkDefs' i rest o'
  , ConstructLinkDefs r v
  , Row.Cons key v o' o
  ) =>
  ConstructLinkDefs' i (RL.Cons key { | r } rest) o

---
class GetAllKeys :: forall k1 k2. k1 -> k2 -> Constraint
class GetAllKeys i o | i -> o

class GetAllKeys' :: forall k1 k2 k3. k1 -> k2 -> k3 -> Constraint
class GetAllKeys' i rl o | i rl -> o

instance (RL.RowToList i rl, GetAllKeys' i rl o) => GetAllKeys i o

instance GetAllKeys' i RL.Nil ()

instance
  ( GetAllKeys' i rest o'
  , Row.Cons key Unit o' o
  ) =>
  GetAllKeys' i (RL.Cons key (IonRouteDefinition links args) rest) o

instance
  ( GetAllKeys' i rest o'
  , Row.Union r o' o
  ) =>
  GetAllKeys' i (RL.Cons key { | r } rest) o

-----
class AddLinksToRoutes :: forall k1 k2 k3. k1 -> k2 -> k3 -> Constraint
class AddLinksToRoutes l i o | l i -> o

class AddLinksToRoutes' :: forall k. Type -> k -> Row Type -> Constraint
class AddLinksToRoutes' l rl o | l rl -> o

instance (RL.RowToList i rl, AddLinksToRoutes' l rl o) => AddLinksToRoutes l i o

instance AddLinksToRoutes' l RL.Nil ()

instance
  ( AddLinksToRoutes' l rest o'
  , Row.Cons key (IonRouteDefinition l args) o' o
  ) =>
  AddLinksToRoutes' l (RL.Cons key (IonRouteDefinition t args) rest) o

instance
  ( AddLinksToRoutes' l rest o'
  , Row.Union r o' o
  ) =>
  AddLinksToRoutes' l (RL.Cons key { | r } rest) o

-------
makeLinks
  :: forall i rl o
   . RL.RowToList i rl
  => MakeLinks' rl o
  => { | i }
  -> { | o }
makeLinks _ = makeLinks' (Proxy :: Proxy rl)

class MakeLinks' :: forall k. k -> Row Type -> Constraint
class MakeLinks' rl o | rl -> o where
  makeLinks' :: Proxy rl -> { | o }

instance MakeLinks' RL.Nil () where
  makeLinks' _ = {}

instance
  ( MakeLinks' rest o'
  , IsSymbol key
  , Yoga.WriteForeign args
  , Row.Lacks key o'
  , Row.Cons key (Poll args -> Array (Poll (Attribute (HTMLIonRouterLink ()))) -> Array Nut -> Nut) o' o
  ) =>
  MakeLinks' (RL.Cons key (IonRouteDefinition links args) rest) o where
  makeLinks' _ = insert (Proxy :: Proxy key) (\u i -> elementify Nothing "ion-router-link" ([ map (doEncode >>> attributeAtYourOwnRisk "href") u ] <> i)) rest
    where
    routeName = reflectSymbol (Proxy :: Proxy key)
    doEncode s = routeName <> "?" <>
      ( unsafePartial $ fromJust $ encode $ fromArray
          [ Tuple "q" (Just (writeJSON s))
          ]
      )
    rest = makeLinks' (Proxy :: Proxy rest)

instance
  ( MakeLinks' rest o'
  , RL.RowToList r rl
  , MakeLinks' rl v
  , IsSymbol key
  , Row.Lacks key o'
  , Row.Cons key { | v } o' o
  ) =>
  MakeLinks' (RL.Cons key { | r } rest) o where
  makeLinks' _ = insert (Proxy :: Proxy key) (makeLinks' (Proxy :: Proxy rl)) rest
    where
    rest = makeLinks' (Proxy :: Proxy rest)

-------------

class MakeRouter links i where
  makeRouter :: { | links } -> { | i } -> Effect (Array Nut)

instance (RL.RowToList i rl, MakeRouter' links i rl) => MakeRouter links i where
  makeRouter links ir = makeRouter' links ir (Proxy :: Proxy rl)

class MakeRouter' :: forall k. Row Type -> Row Type -> k -> Constraint
class MakeRouter' links i rl where
  makeRouter' :: { | links } -> { | i } -> Proxy rl -> Effect (Array Nut)

instance MakeRouter' links i RL.Nil where
  makeRouter' _ _ _ = pure []

instance
  ( MakeRouter' links i rest
  , IsSymbol key
  , Yoga.ReadForeign args
  , Row.Cons key (IonRouteDefinition { | links } args) i' i
  ) =>
  MakeRouter' links i (RL.Cons key (IonRouteDefinition { | links } args) rest) where
  makeRouter' links ir _ = do
    let IonRouteDefinition ctor = Record.get (Proxy :: Proxy key) ir
    eltName <- generateEltName
    unsafeCustomComponent unsafeJSON eltName mempty mempty (ctor links)
    rest <- makeRouter' links ir (Proxy :: Proxy rest)
    pure $ [ elementify Nothing "ion-route" [ pure (attributeAtYourOwnRisk "component" eltName), pure (attributeAtYourOwnRisk "url" routeName) ] [] ] <> rest
    where
    routeName = reflectSymbol (Proxy :: Proxy key)
    unsafeJSON s = unsafePartial $ fromJust $ readJSON_ s

instance
  ( MakeRouter' links i rest
  , RL.RowToList r rl
  , MakeRouter' links r rl
  , IsSymbol key
  , Row.Cons key { | r } i' i
  ) =>
  MakeRouter' links i (RL.Cons key { | r } rest) where
  makeRouter' links ir _ = do
    eltName <- generateEltName
    toInsert <- makeRouter' links (Record.get (Proxy :: Proxy key) ir) (Proxy :: Proxy rl)
    rest <- makeRouter' links ir (Proxy :: Proxy rest)
    pure $ [ elementify Nothing "ion-route" [ pure (attributeAtYourOwnRisk "component" eltName), pure (attributeAtYourOwnRisk "url" (reflectSymbol (Proxy :: Proxy key))) ] toInsert ] <> rest

ionRoute_ :: forall links @args. (links -> args -> Nut) -> IonRouteDefinition links args
ionRoute_ = IonRouteDefinition

ionRouter
  :: forall rl links i o k
   . RowToList i rl
  => GetAllKeys i k
  => Row.Nub k k
  => MakeLinks' rl links
  => AddLinksToRoutes (Record links) i o
  => TE.TypeEquals { | i } { | o }
  => MakeRouter links o
  => Array (Poll (Attribute (HTMLIonRouter ())))
  -> { | i }
  -> Effect Nut
ionRouter atts routes = do
  let links = makeLinks routes
  let rproof = TE.to routes
  elementify Nothing "ion-router" atts <$> makeRouter links rproof

ionRouter_
  :: forall rl links i o k
   . RowToList i rl
  => GetAllKeys i k
  => Row.Nub k k
  => MakeLinks' rl links
  => AddLinksToRoutes (Record links) i o
  => TE.TypeEquals { | i } { | o }
  => MakeRouter links o
  => { | i }
  -> Effect Nut
ionRouter_ = ionRouter []
