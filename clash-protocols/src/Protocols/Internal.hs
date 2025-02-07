{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=20 #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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

import qualified Clash.Explicit.Prelude as CE
import Clash.Prelude (type (*), type (+))
import qualified Clash.Prelude as C

import Protocols.Internal.TH (
  backPressureTupleInstances,
  drivableTupleInstances,
  simulateTupleInstances,
 )
import Protocols.Internal.Types
import Protocols.Plugin
import Protocols.Plugin.Cpp (maxTupleSize)
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
  deriving (Generic, C.NFDataX, Show, C.Bundle, Eq, Ord)

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

instance Backpressure () where
  boolsToBwd _ _ = ()

instance (Backpressure a, Backpressure b) => Backpressure (a, b) where
  boolsToBwd _ bs = (boolsToBwd (Proxy @a) bs, boolsToBwd (Proxy @b) bs)

backPressureTupleInstances 3 maxTupleSize

instance (C.KnownNat n, Backpressure a) => Backpressure (C.Vec n a) where
  boolsToBwd _ bs = C.repeat (boolsToBwd (Proxy @a) bs)

instance Backpressure (CSignal dom a) where
  boolsToBwd _ _ = pure ()

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

{- | Combine two separate circuits into one. If you are looking to combine
multiple streams into a single stream, checkout 'Protocols.Df.fanin'.
-}
prod2C ::
  forall a c b d.
  Circuit a b ->
  Circuit c d ->
  Circuit (a, c) (b, d)
prod2C (Circuit a) (Circuit c) =
  Circuit
    ( \((aFwd, cFwd), (bBwd, dBwd)) ->
        let
          (aBwd, bFwd) = a (aFwd, bBwd)
          (cBwd, dFwd) = c (cFwd, dBwd)
         in
          ((aBwd, cBwd), (bFwd, dFwd))
    )

--------------------------------- SIMULATION -----------------------------------

instance Simulate () where
  type SimulateFwdType () = ()
  type SimulateBwdType () = ()
  type SimulateChannels () = 0

  simToSigFwd _ = id
  simToSigBwd _ = id
  sigToSimFwd _ = id
  sigToSimBwd _ = id

  stallC _ _ = idC

instance Drivable () where
  type ExpectType () = ()

  toSimulateType Proxy () = ()
  fromSimulateType Proxy () = ()

  driveC _ _ = idC
  sampleC _ _ = ()

instance (Simulate a, Simulate b) => Simulate (a, b) where
  type SimulateFwdType (a, b) = (SimulateFwdType a, SimulateFwdType b)
  type SimulateBwdType (a, b) = (SimulateBwdType a, SimulateBwdType b)
  type SimulateChannels (a, b) = SimulateChannels a + SimulateChannels b

  simToSigFwd Proxy (fwdsA, fwdsB) = (simToSigFwd (Proxy @a) fwdsA, simToSigFwd (Proxy @b) fwdsB)
  simToSigBwd Proxy (bwdsA, bwdsB) = (simToSigBwd (Proxy @a) bwdsA, simToSigBwd (Proxy @b) bwdsB)
  sigToSimFwd Proxy (fwdSigA, fwdSigB) = (sigToSimFwd (Proxy @a) fwdSigA, sigToSimFwd (Proxy @b) fwdSigB)
  sigToSimBwd Proxy (bwdSigA, bwdSigB) = (sigToSimBwd (Proxy @a) bwdSigA, sigToSimBwd (Proxy @b) bwdSigB)

  stallC conf stalls =
    let
      (stallsL, stallsR) = C.splitAtI @(SimulateChannels a) @(SimulateChannels b) stalls
      Circuit stalledL = stallC @a conf stallsL
      Circuit stalledR = stallC @b conf stallsR
     in
      Circuit $ \((fwdL0, fwdR0), (bwdL0, bwdR0)) ->
        let
          (fwdL1, bwdL1) = stalledL (fwdL0, bwdL0)
          (fwdR1, bwdR1) = stalledR (fwdR0, bwdR0)
         in
          ((fwdL1, fwdR1), (bwdL1, bwdR1))

simulateTupleInstances 3 maxTupleSize

instance (Drivable a, Drivable b) => Drivable (a, b) where
  type ExpectType (a, b) = (ExpectType a, ExpectType b)

  toSimulateType Proxy (t1, t2) =
    ( toSimulateType (Proxy @a) t1
    , toSimulateType (Proxy @b) t2
    )

  fromSimulateType Proxy (t1, t2) =
    ( fromSimulateType (Proxy @a) t1
    , fromSimulateType (Proxy @b) t2
    )

  driveC conf (fwd1, fwd2) =
    let (Circuit f1, Circuit f2) = (driveC @a conf fwd1, driveC @b conf fwd2)
     in Circuit (\(_, ~(bwd1, bwd2)) -> ((), (snd (f1 ((), bwd1)), snd (f2 ((), bwd2)))))

  sampleC conf (Circuit f) =
    let
      bools = replicate (resetCycles conf) False <> repeat True
      (_, (fwd1, fwd2)) = f ((), (boolsToBwd (Proxy @a) bools, boolsToBwd (Proxy @b) bools))
     in
      ( sampleC @a conf (Circuit $ \_ -> ((), fwd1))
      , sampleC @b conf (Circuit $ \_ -> ((), fwd2))
      )

drivableTupleInstances 3 maxTupleSize

instance (CE.KnownNat n, Simulate a) => Simulate (C.Vec n a) where
  type SimulateFwdType (C.Vec n a) = C.Vec n (SimulateFwdType a)
  type SimulateBwdType (C.Vec n a) = C.Vec n (SimulateBwdType a)
  type SimulateChannels (C.Vec n a) = n * SimulateChannels a

  simToSigFwd Proxy = C.map (simToSigFwd (Proxy @a))
  simToSigBwd Proxy = C.map (simToSigBwd (Proxy @a))
  sigToSimFwd Proxy = C.map (sigToSimFwd (Proxy @a))
  sigToSimBwd Proxy = C.map (sigToSimBwd (Proxy @a))

  stallC conf stalls0 =
    let
      stalls1 = C.unconcatI @n @(SimulateChannels a) stalls0
      stalled = C.map (toSignals . stallC @a conf) stalls1
     in
      Circuit $ \(fwds, bwds) -> C.unzip (C.zipWith ($) stalled (C.zip fwds bwds))

instance (C.KnownNat n, Drivable a) => Drivable (C.Vec n a) where
  type ExpectType (C.Vec n a) = C.Vec n (ExpectType a)

  toSimulateType Proxy = C.map (toSimulateType (Proxy @a))
  fromSimulateType Proxy = C.map (fromSimulateType (Proxy @a))

  driveC conf fwds =
    let circuits = C.map (($ ()) . curry . (toSignals @_ @a) . driveC conf) fwds
     in Circuit (\(_, bwds) -> ((), C.map snd (C.zipWith ($) circuits bwds)))

  sampleC conf (Circuit f) =
    let
      bools = replicate (resetCycles conf) False <> repeat True
      (_, fwds) = f ((), (C.repeat (boolsToBwd (Proxy @a) bools)))
     in
      C.map (\fwd -> sampleC @a conf (Circuit $ \_ -> ((), fwd))) fwds

instance (C.KnownDomain dom) => Simulate (CSignal dom a) where
  type SimulateFwdType (CSignal dom a) = [a]
  type SimulateBwdType (CSignal dom a) = ()
  type SimulateChannels (CSignal dom a) = 1

  simToSigFwd Proxy list = C.fromList_lazy list
  simToSigBwd Proxy () = pure ()
  sigToSimFwd Proxy sig = C.sample_lazy sig
  sigToSimBwd Proxy _ = ()

  stallC _ _ = idC

instance (C.NFDataX a, C.ShowX a, Show a, C.KnownDomain dom) => Drivable (CSignal dom a) where
  type ExpectType (CSignal dom a) = [a]

  toSimulateType Proxy = id
  fromSimulateType Proxy = id

  driveC _conf [] = error "CSignal.driveC: Can't drive with empty list"
  driveC SimulationConfig{resetCycles} fwd0@(f : _) =
    let fwd1 = C.fromList_lazy (replicate resetCycles f <> fwd0 <> repeat f)
     in Circuit (\_ -> ((), fwd1))

  sampleC SimulationConfig{resetCycles, ignoreReset} (Circuit f) =
    let sampled = CE.sample_lazy (snd (f ((), pure ())))
     in if ignoreReset then drop resetCycles sampled else sampled

{- | Simulate a circuit. Includes samples while reset is asserted.
Not synthesizable.

To figure out what input you need to supply, either solve the type
"SimulateFwdType" manually, or let the repl do the work for you! Example:

>>> :kind! (forall dom a. SimulateFwdType (Df dom a))
...
= [Protocols.Df.Data a]

This would mean a @Circuit (Df dom a) (Df dom b)@ would need
@[Data a]@ as the last argument of 'simulateC' and would result in
@[Data b]@. Note that for this particular type you can neither supply
stalls nor introduce backpressure. If you want to to this use 'Df.stall'.
-}
simulateC ::
  forall a b.
  (Drivable a, Drivable b) =>
  -- | Circuit to simulate
  Circuit a b ->
  -- | Simulation configuration. Note that some options only apply to 'sampleC'
  -- and some only to 'driveC'.
  SimulationConfig ->
  -- | Circuit input
  SimulateFwdType a ->
  -- | Circuit output
  SimulateFwdType b
simulateC c conf as =
  sampleC conf (driveC conf as |> c)

{- | Like 'simulateC', but does not allow caller to control and observe
backpressure. Furthermore, it ignores all data produced while the reset is
asserted.

Example:

>>> import qualified Protocols.Df as Df
>>> take 2 (simulateCS (Df.catMaybes @C.System @Int) [Nothing, Just 1, Nothing, Just 3])
[1,3]
-}
simulateCS ::
  forall a b.
  (Drivable a, Drivable b) =>
  -- | Circuit to simulate
  Circuit a b ->
  -- | Circuit input
  ExpectType a ->
  -- | Circuit output
  ExpectType b
simulateCS c =
  fromSimulateType (Proxy @b)
    . simulateC c def{ignoreReset = True}
    . toSimulateType (Proxy @a)

-- | Like 'simulateCS', but takes a circuit expecting a clock, reset, and enable.
simulateCSE ::
  forall dom a b.
  (Drivable a, Drivable b, C.KnownDomain dom) =>
  -- | Circuit to simulate
  (C.Clock dom -> C.Reset dom -> C.Enable dom -> Circuit a b) ->
  -- | Circuit input
  ExpectType a ->
  -- | Circuit output
  ExpectType b
simulateCSE c = simulateCS (c clk rst ena)
 where
  clk = C.clockGen
  rst = resetGen (resetCycles def)
  ena = C.enableGen

  resetGen n =
    C.unsafeFromActiveHigh $
      C.fromList (replicate n True <> repeat False)

{- | Applies conversion functions defined in the 'Simulate' instance of @a@ and @b@ to
the given simulation types, and applies the results to the internal function of the
given 'Circuit'. The resulting internal types are converted to the simulation types.
-}
simulateCircuit ::
  forall a b.
  (Simulate a, Simulate b) =>
  SimulateFwdType a ->
  SimulateBwdType b ->
  Circuit a b ->
  (SimulateBwdType a, SimulateFwdType b)
simulateCircuit fwds bwds circ =
  (sigToSimBwd (Proxy @a) bwdSig, sigToSimFwd (Proxy @b) fwdSig)
 where
  (bwdSig, fwdSig) =
    toSignals circ $
      (simToSigFwd (Proxy @a) fwds, simToSigBwd (Proxy @b) bwds)

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

  -- | Convert an optional value to a normal value,
  -- or Nothing if the field is turned off.
  fromKeepType :: KeepType keep optionalType -> Maybe optionalType

  -- | Convert a normal value to an optional value.
  -- Either preserves the value or returns @Proxy@.
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

{- | "Bundle" together a pair of 'Circuit's into a 'Circuit' with two inputs and outputs.
The 'Circuit's run in parallel.
-}
tupCircuits :: Circuit a b -> Circuit c d -> Circuit (a, c) (b, d)
tupCircuits (Circuit f) (Circuit g) = Circuit (reorder . (f *** g) . reorder)
 where
  reorder ~(~(a, b), ~(c, d)) = ((a, c), (b, d))
