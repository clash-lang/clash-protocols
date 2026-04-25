{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=20 #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fplugin Protocols.Plugin #-}

-- TODO: Hide internal documentation
-- {-# OPTIONS_HADDOCK hide #-}

{- |
Internal module to prevent hs-boot files (breaks Haddock)
-}
module Protocols.Internal (
  module Protocols.Internal,
  module Protocols.Internal.Types,
  module Protocols.Plugin,
  module Protocols.Plugin.Units,
  module Protocols.Plugin.TaggedBundle,
) where

import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe)
import Data.Proxy
import Prelude hiding (const, map)

import Clash.Prelude qualified as C

import Protocols.Internal.Types
import Protocols.Plugin
import Protocols.Plugin.TaggedBundle
import Protocols.Plugin.Units

import Control.Arrow ((***))
import Data.Coerce (coerce)
import Data.Default (Default (def))
import Data.Functor.Identity (Identity (..), runIdentity)
import Data.Kind (Type)
import Data.Tuple (swap)
import GHC.Generics (Generic)

{- $setup
>>> import Protocols
-}

-- | Protocol-agnostic acknowledgement
newtype Ack = Ack Bool
  deriving stock (Generic, Show)
  deriving anyclass (C.Bundle, C.ShowX)
  deriving newtype (C.NFDataX, Eq, Ord, C.BitPack)

-- | Acknowledge. Used in circuit-notation plugin to drive ignore components.
instance Default Ack where
  def = Ack True

{- | Left-to-right circuit composition.

@
                       Circuit a c

           +---------------------------------+

            Circuit a b    |>    Circuit b c

           +-----------+         +-----------+
    Fwd a  |           |  Fwd b  |           |  Fwd c
  +------->+           +-------->+           +-------->
           |           |         |           |
           |           |         |           |
    Bwd a  |           |  Bwd b  |           |  Bwd c
  <--------+           +<--------+           +<-------+
           |           |         |           |
           +-----------+         +-----------+
@
-}
infixr 1 |>

(|>) :: Circuit a b -> Circuit b c -> Circuit a c
(Circuit fab) |> (Circuit fbc) = Circuit $ \(s2rAc, r2sAc) ->
  let
    ~(r2sAb, s2rAb) = fab (s2rAc, r2sBc)
    ~(r2sBc, s2rBc) = fbc (s2rAb, r2sAc)
   in
    (r2sAb, s2rBc)

{- | Right-to-left circuit composition.

@
                       Circuit a c

           +---------------------------------+

            Circuit b c    <|    Circuit a b

           +-----------+         +-----------+
    Fwd c  |           |  Fwd b  |           |  Fwd a
  <--------+           +<--------+           +<-------+
           |           |         |           |
           |           |         |           |
    Bwd c  |           |  Bwd b  |           |  Bwd a
  +------->+           +-------->+           +-------->
           |           |         |           |
           +-----------+         +-----------+
@
-}
infixr 1 <|

(<|) :: Circuit b c -> Circuit a b -> Circuit a c
(<|) = flip (|>)

-- | View Circuit as its internal representation.
toSignals :: Circuit a b -> ((Fwd a, Bwd b) -> (Bwd a, Fwd b))
toSignals = coerce

-- | View signals as a Circuit
fromSignals :: ((Fwd a, Bwd b) -> (Bwd a, Fwd b)) -> Circuit a b
fromSignals = coerce

{- | Circuit equivalent of 'id'. Useful for explicitly assigning a type to
another protocol, or to return a result when using the circuit-notation
plugin.

Examples:

@
idC \@(Df dom a) <| somePolymorphicProtocol
@

@
swap :: Circuit (Df dom a, Df dom b) (Df dom b, Df dom a)
swap = circuit $ \(a, b) -> do
  idC -< (b, a)
@
-}
idC :: forall a. Circuit a a
idC = Circuit swap

{- | Copy a circuit /n/ times. Note that this will copy hardware. If you are
looking for a circuit that turns a single channel into multiple, check out
'Protocols.Df.fanout'.
-}
repeatC ::
  forall n a b.
  Circuit a b ->
  Circuit (C.Vec n a) (C.Vec n b)
repeatC (Circuit f) =
  Circuit (C.unzip . C.map f . uncurry C.zip)

{- | Applies the mappings @Fwd a -> Fwd b@ and @Bwd b -> Bwd a@ to the circuit's signals.

The idea here is that you want to treat some @a -> b@ as a @Circuit a b@, but @Circuit a b@ is
actually the type @(Fwd a, Bwd b) -> (Bwd a, Fwd b)@. To bridge this gap, we say that @a -> b@ can
be our map from @Fwd a -> Fwd b@, but then we need to fill in the @Bwd b -> Bwd a@ still. In almost
all cases, the former is the function you want to apply, and the latter is the inverse. For
instance, the 'Clash.Prelude.++' operator on vectors can be made into a @Circuit a b@ with
@applyC (uncurry (++)) splitAtI@, since 'Clash.Prelude.splitAtI' is the inverse of
'Clash.Prelude.++'.
-}
applyC ::
  forall a b.
  (Fwd a -> Fwd b) ->
  (Bwd b -> Bwd a) ->
  Circuit a b
applyC fwdFn bwdFn = Circuit go
 where
  go :: (Fwd a, Bwd b) -> (Bwd a, Fwd b)
  go (fwdA, bwdB) = (bwdFn bwdB, fwdFn fwdA)

{- | Combine two separate circuits into one. If you are looking to combine
multiple streams into a single stream, checkout 'Protocols.Df.fanin'.
-}
prod2C ::
  forall a c b d.
  Circuit a b ->
  Circuit c d ->
  Circuit (a, c) (b, d)
prod2C ab cd = circuit $ \(a, c) -> do
  b <- ab -< a
  d <- cd -< c
  idC -< (b, d)

{- | Combine three separate circuits into one. If you are looking to combine
multiple streams into a single stream, checkout 'Protocols.Df.fanin'.
-}
prod3C ::
  forall a c b d e f.
  Circuit a b ->
  Circuit c d ->
  Circuit e f ->
  Circuit (a, c, e) (b, d, f)
prod3C ab cd ef = circuit $ \(a, c, e) -> do
  b <- ab -< a
  d <- cd -< c
  f <- ef -< e
  idC -< (b, d, f)

{- | Combine four separate circuits into one. If you are looking to combine
multiple streams into a single stream, checkout 'Protocols.Df.fanin'.
-}
prod4C ::
  forall a c b d e f g h.
  Circuit a b ->
  Circuit c d ->
  Circuit e f ->
  Circuit g h ->
  Circuit (a, c, e, g) (b, d, f, h)
prod4C ab cd ef gh = circuit $ \(a, c, e, g) -> do
  b <- ab -< a
  d <- cd -< c
  f <- ef -< e
  h <- gh -< g
  idC -< (b, d, f, h)

{- | Allows for optional data.
Depending on the value of @keep@, the data can either be included or left out.
When left out, the data is represented instead as type @()@.
-}
type family KeepType (keep :: Bool) (optionalType :: Type) = t | t -> keep optionalType where
  KeepType 'True optionalType = Identity optionalType
  KeepType 'False optionalType = Proxy optionalType

#if !MIN_VERSION_clash_prelude(1, 8, 2)
deriving instance (C.ShowX t) => (C.ShowX (Proxy t))
deriving instance (C.NFDataX t) => (C.NFDataX (Proxy t))
#endif

{- | We want to define operations on 'KeepType' that work for both possibilities
(@keep = 'True@ and @keep = 'False@), but we can't pattern match directly.
Instead we need to define a class and instantiate
the class for both @'True@ and @'False@.
-}
class
  ( Eq (KeepType keep Bool)
  , Show (KeepType keep Bool)
  , C.ShowX (KeepType keep Bool)
  , NFData (KeepType keep Bool)
  , C.NFDataX (KeepType keep Bool)
  , Hashable (KeepType keep Bool)
  ) =>
  KeepTypeClass (keep :: Bool)
  where
  -- | Get the value of @keep@ at the term level.
  getKeep :: KeepType keep optionalType -> Bool

  {- | Convert an optional value to a normal value,
  or Nothing if the field is turned off.
  -}
  fromKeepType :: KeepType keep optionalType -> Maybe optionalType

  {- | Convert a normal value to an optional value.
  Either preserves the value or returns @Proxy@.
  -}
  toKeepType :: optionalType -> KeepType keep optionalType

  -- | Map a function over an optional value
  mapKeepType ::
    (optionalType -> optionalType) -> KeepType keep optionalType -> KeepType keep optionalType

instance KeepTypeClass 'True where
  getKeep _ = True
  fromKeepType i = Just (runIdentity i)
  toKeepType v = Identity v
  mapKeepType = fmap

instance KeepTypeClass 'False where
  getKeep _ = False
  fromKeepType _ = Nothing
  toKeepType _ = Proxy
  mapKeepType = fmap

{- | Grab a value from 'KeepType', given a default value. Uses 'fromMaybe'
and 'fromKeepType'.
-}
fromKeepTypeDef ::
  (KeepTypeClass keep) =>
  optionalType ->
  KeepType keep optionalType ->
  optionalType
fromKeepTypeDef deflt val = fromMaybe deflt (fromKeepType val)

{- | Convert one optional field to another, keeping the value the same if
possible. If not possible, a default argument is provided.
-}
convKeepType ::
  (KeepTypeClass a, KeepTypeClass b) => t -> KeepType a t -> KeepType b t
convKeepType b = toKeepType . fromKeepTypeDef b

-- | Omitted value in @KeepType 'False t@.
keepTypeFalse :: KeepType 'False t
keepTypeFalse = Proxy

-- | Grab value in @KeepType 'True t@. No default is needed.
fromKeepTypeTrue :: KeepType 'True t -> t
fromKeepTypeTrue = runIdentity

{- | Protocol to reverse a circuit.
'Fwd' becomes 'Bwd' and vice versa.
No changes are made otherwise.
-}
data Reverse a

instance (Protocol a) => Protocol (Reverse a) where
  type Fwd (Reverse a) = Bwd a
  type Bwd (Reverse a) = Fwd a

{- | Apply 'Reverse' both sides of a circuit, and then switch them.
Input and output of the underlying circuit are the same,
but with the order of the tuple switched in both cases.
-}
reverseCircuit :: Circuit a b -> Circuit (Reverse b) (Reverse a)
reverseCircuit ckt = Circuit (swap . toSignals ckt . swap)

{- | If two protocols, @a@ and @a'@, have the same 'Fwd' and 'Bwd' values,
convert a @Circuit a@ to a @Circuit a'@ without changing the underlying function at all.
-}
coerceCircuit ::
  (Fwd a ~ Fwd a', Bwd a ~ Bwd a', Fwd b ~ Fwd b', Bwd b ~ Bwd b') =>
  Circuit a b ->
  Circuit a' b'
coerceCircuit (Circuit f) = Circuit f

{- | Change a circuit by changing its underlying function's inputs and outputs.
It takes 4 functions as input: @ia@, @oa@, @ob@, and @ib@.
@ia@ modifies the 'Bwd' input, @ib@ modifies the 'Fwd' input,
@oa@ modifies the 'Bwd' output, and @ob@ modifies the 'Fwd' output.
-}
mapCircuit ::
  (Fwd a' -> Fwd a) ->
  (Bwd a -> Bwd a') ->
  (Fwd b -> Fwd b') ->
  (Bwd b' -> Bwd b) ->
  Circuit a b ->
  Circuit a' b'
mapCircuit ia oa ob ib (Circuit f) = Circuit ((oa *** ob) . f . (ia *** ib))

{- | "Bundle" together a pair of t'Circuit's into a t'Circuit' with two inputs and outputs.
The t'Circuit's run in parallel.
-}
tupCircuits :: Circuit a b -> Circuit c d -> Circuit (a, c) (b, d)
tupCircuits (Circuit f) (Circuit g) = Circuit (reorder . (f *** g) . reorder)
 where
  reorder ~(~(a, b), ~(c, d)) = ((a, c), (b, d))

-- | Can be inserted between a manager and subordinate to monitor their interaction
circuitMonitor ::
  (Protocol p, Fwd p ~ C.Signal dom fwd, Bwd p ~ C.Signal dom bwd) =>
  Circuit p (p, CSignal dom (fwd, bwd))
circuitMonitor = Circuit (\ ~(fwd, (bwd, _)) -> (bwd, (fwd, C.bundle (fwd, bwd))))
