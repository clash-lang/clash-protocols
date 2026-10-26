{- | Module for defining stateful tests of circuits (called "drivers").

The tests are expressed in a monadic style with the fundamental operation
being a "step" of the simulation. The defined tests are working monad
transformers so you can run in whatever monad you want (for example Hedgehog!)
-}
module Protocols.Experimental.Hedgehog.Driver (
  ProtocolSignals (..),
  CircuitDriverInstructions,
  CircuitDriverT,
  CircuitDriver,
  runCircuitDriverT,
  runCircuitDriver,
  step,
  getConstFwdA,
  getConstFwdB,
  getConstBwdB,
  circuitLhsToSignals,
) where

import Clash.Prelude hiding (fromSignal, singleton)
import Control.Arrow.Transformer.Automaton (Automaton (Automaton))
import Control.Monad.Identity (runIdentity)
import Control.Monad.Operational
import Data.Data (Proxy (Proxy))
import Protocols
import Protocols.Experimental.ReqResp (ReqResp)
import Protocols.Experimental.Wishbone (Wishbone, WishboneM2S, WishboneS2M)

{- | Type-class that allows a protocol's 'Fwd' and 'Bwd' to be converted to
or recovered from signals.

This is useful so that any `Circuit a ()` can be turned into a function
`Signal dom (FwdSignalInner a) -> Signal dom (BwdSignalInner a)`.
 See 'circuitLhsToSignals' for that.

The @dom@ parameter is needed because not all 'Protocol's represent their
'Fwd' and 'Bwd' channels with signals.
-}
class (Protocol p) => ProtocolSignals dom p where
  -- | Constant data of the forward channel. This data does not change over time. (e.g. `ToConst a`)
  type FwdConst p

  -- | Constant data on the backward channel. This data does not change over time (e.g. `ToConstBwd a`)
  type BwdConst p

  -- | Dynamic data on the forward channel that can change per clock cycle.
  type FwdSignalInner p

  -- | Dynamic data on the backward channel that can change per clock cycle.
  type BwdSignalInner p

  fwdFromSignal :: Proxy p -> FwdConst p -> Signal dom (FwdSignalInner p) -> Fwd p
  signalFromFwd ::
    (Proxy p, Proxy dom) -> FwdConst p -> Fwd p -> Signal dom (FwdSignalInner p)

  bwdFromSignal :: Proxy p -> BwdConst p -> Signal dom (BwdSignalInner p) -> Bwd p
  signalFromBwd :: (Proxy p, Proxy dom) -> Bwd p -> Signal dom (BwdSignalInner p)

{- | The "instructions" for a driver. These are the building blocks
for monadic operations.
-}
data CircuitDriverInstructions dom a b res where
  GetConstFwdA :: CircuitDriverInstructions dom a b (FwdConst a)
  GetConstFwdB :: CircuitDriverInstructions dom a b (FwdConst b)
  GetConstBwdB :: CircuitDriverInstructions dom a b (BwdConst b)
  Step ::
    FwdSignalInner a ->
    BwdSignalInner b ->
    CircuitDriverInstructions dom a b (BwdSignalInner a, FwdSignalInner b)

-- | Circuit driver program in the monad @m@, computing a @res@ value.
type CircuitDriverT dom a b m res = ProgramT (CircuitDriverInstructions dom a b) m res

-- | A non-monadic circuit driver program, computing a @res@ value.
type CircuitDriver dom a b res = Program (CircuitDriverInstructions dom a b) res

-- | Turn a circuit into a function of 'FwdSignalInner' and 'BwdSignalInner' signals.
circuitLhsToSignals ::
  forall dom a b.
  (ProtocolSignals dom a, ProtocolSignals dom b) =>
  Circuit a b ->
  FwdConst a ->
  FwdConst b ->
  BwdConst b ->
  Signal dom (FwdSignalInner a, BwdSignalInner b) ->
  Signal dom (BwdSignalInner a, FwdSignalInner b)
circuitLhsToSignals (Circuit fn) cAFwd cBFwd cBBwd input = bundle (bwdASig, fwdBSig)
 where
  (fwdASigInner, bwdBSigInner) = unbundle input
  a = fwdFromSignal (Proxy @a) cAFwd fwdASigInner
  b = bwdFromSignal (Proxy @b) cBBwd bwdBSigInner

  (bwdA, fwdB) = fn (a, b)
  bwdASig = signalFromBwd (Proxy @a, Proxy @dom) bwdA
  fwdBSig = signalFromFwd (Proxy @b, Proxy @dom) cBFwd fwdB

-- | Run a circuit driver in the 'Monad' @m@.
runCircuitDriverT ::
  forall dom a b m res.
  (Monad m) =>
  (KnownDomain dom, ProtocolSignals dom a, ProtocolSignals dom b) =>
  -- | Circuit to be simulated/driven
  Circuit a b ->
  FwdConst a ->
  FwdConst b ->
  BwdConst b ->
  -- | driver "program" to run
  CircuitDriverT dom a b m res ->
  m res
runCircuitDriverT circ cAFwd cBFwd cBBwd prog = run (signalAutomaton @dom $ circuitLhsToSignals circ cAFwd cBFwd cBBwd) prog
 where
  run ::
    Automaton (->) (FwdSignalInner a, BwdSignalInner b) (BwdSignalInner a, FwdSignalInner b) ->
    CircuitDriverT dom a b m res ->
    m res
  run currAutomaton@(Automaton step0) driver = do
    driverView <- viewT driver
    case driverView of
      Return val -> pure val
      GetConstFwdA :>>= next -> do
        let x = next cAFwd
        run currAutomaton x
      GetConstFwdB :>>= next -> do
        let x = next cBFwd
        run currAutomaton x
      GetConstBwdB :>>= next -> do
        let x = next cBBwd
        run currAutomaton x
      Step fwd0 bwd0 :>>= next -> do
        let ((bwd1, fwd1), nextAutomaton) = step0 (fwd0, bwd0)
        run nextAutomaton (next (bwd1, fwd1))

-- | Run a pure circuit driver.
runCircuitDriver ::
  forall dom a b res.
  (KnownDomain dom, ProtocolSignals dom a, ProtocolSignals dom b) =>
  Circuit a b ->
  FwdConst a ->
  FwdConst b ->
  BwdConst b ->
  CircuitDriver dom a b res ->
  res
runCircuitDriver circ cAFwd cBFwd cBBwd prog = runIdentity $ runCircuitDriverT circ cAFwd cBFwd cBBwd prog

{- | Perform a simulation step of the circuit.

This will provide @fwd@ as the input and will give the @bwd@ value
as the monadic binder.
-}
step ::
  FwdSignalInner a ->
  BwdSignalInner b ->
  CircuitDriverT dom a b m (BwdSignalInner a, FwdSignalInner b)
step fwd bwd = singleton $ Step fwd bwd

-- | Get the constant forward data for the LHS-protocol of the current circuit
getConstFwdA :: CircuitDriverT dom a b m (FwdConst a)
getConstFwdA = singleton $ GetConstFwdA

-- | Get the constant forward data for the RHS-protocol of the current circuit
getConstFwdB :: CircuitDriverT dom a b m (FwdConst b)
getConstFwdB = singleton $ GetConstFwdB

-- | Get the constant forward data for the RHS-protocol of the current circuit
getConstBwdB :: CircuitDriverT dom a b m (BwdConst b)
getConstBwdB = singleton $ GetConstBwdB

--
-- Some instances for `ProtocolSignals`
--

instance ProtocolSignals dom () where
  type FwdConst () = ()
  type BwdConst () = ()
  type FwdSignalInner () = ()
  type BwdSignalInner () = ()

  fwdFromSignal _ () _ = ()
  signalFromFwd _ () () = pure ()
  bwdFromSignal _ () _ = ()
  signalFromBwd _ () = pure ()

instance ProtocolSignals dom (ToConst a) where
  type FwdConst (ToConst a) = a
  type BwdConst (ToConst a) = ()
  type FwdSignalInner (ToConst a) = ()
  type BwdSignalInner (ToConst a) = ()

  fwdFromSignal _ a _ = a
  signalFromFwd _ _ _ = pure ()
  bwdFromSignal _ () _ = ()
  signalFromBwd _ () = pure ()

instance ProtocolSignals dom (ToConstBwd a) where
  type FwdConst (ToConstBwd a) = ()
  type BwdConst (ToConstBwd a) = a
  type FwdSignalInner (ToConstBwd a) = ()
  type BwdSignalInner (ToConstBwd a) = ()

  fwdFromSignal _ () _ = ()
  signalFromFwd _ _ _ = pure ()
  bwdFromSignal _ a _ = a
  signalFromBwd _ _ = pure ()

instance ProtocolSignals dom (CSignal dom a) where
  type FwdConst (CSignal dom a) = ()
  type BwdConst (CSignal dom a) = ()
  type FwdSignalInner (CSignal dom a) = a
  type BwdSignalInner (CSignal dom a) = ()

  fwdFromSignal _ () s = s
  signalFromFwd _ () s = s
  bwdFromSignal _ () _ = ()
  signalFromBwd _ () = pure ()

instance (KnownNat n, ProtocolSignals dom p) => ProtocolSignals dom (Vec n p) where
  type FwdConst (Vec n p) = FwdConst p
  type BwdConst (Vec n p) = BwdConst p
  type FwdSignalInner (Vec n p) = Vec n (FwdSignalInner p)
  type BwdSignalInner (Vec n p) = Vec n (BwdSignalInner p)

  fwdFromSignal _ c fwds = fwdFromSignal (Proxy @p) c <$> unbundle fwds
  signalFromFwd _ c fwds = bundle $ signalFromFwd (Proxy @p, Proxy @dom) c <$> fwds
  bwdFromSignal _ c bwds = bwdFromSignal (Proxy @p) c <$> unbundle bwds
  signalFromBwd _ bwds = bundle $ signalFromBwd (Proxy @p, Proxy @dom) <$> bwds

instance (ProtocolSignals dom a, ProtocolSignals dom b) => ProtocolSignals dom (a, b) where
  type FwdConst (a, b) = (FwdConst a, FwdConst b)
  type BwdConst (a, b) = (BwdConst a, BwdConst b)
  type FwdSignalInner (a, b) = (FwdSignalInner a, FwdSignalInner b)
  type BwdSignalInner (a, b) = (BwdSignalInner a, BwdSignalInner b)

  fwdFromSignal _ (cA, cB) (unbundle -> (fwdA, fwdB)) =
    (fwdFromSignal (Proxy @a) cA fwdA, fwdFromSignal (Proxy @b) cB fwdB)
  signalFromFwd _ (cA, cB) (fwdA, fwdB) = bundle (a, b)
   where
    a = signalFromFwd (Proxy @a, Proxy @dom) cA fwdA
    b = signalFromFwd (Proxy @b, Proxy @dom) cB fwdB

  bwdFromSignal _ (cA, cB) (unbundle -> (bwdA, bwdB)) = (a, b)
   where
    a = bwdFromSignal (Proxy @a) cA bwdA
    b = bwdFromSignal (Proxy @b) cB bwdB
  signalFromBwd _ (bwdA, bwdB) = bundle (a, b)
   where
    a = signalFromBwd (Proxy @a, Proxy @dom) bwdA
    b = signalFromBwd (Proxy @b, Proxy @dom) bwdB

instance ProtocolSignals dom (Df dom a) where
  type FwdConst (Df dom a) = ()
  type BwdConst (Df dom a) = ()
  type FwdSignalInner (Df dom a) = Maybe a
  type BwdSignalInner (Df dom a) = Ack

  fwdFromSignal _ () s = s
  signalFromFwd _ () s = s
  bwdFromSignal _ () s = s
  signalFromBwd _ bwd = bwd

instance ProtocolSignals dom (ReqResp dom req resp) where
  type FwdConst (ReqResp dom req resp) = ()
  type BwdConst (ReqResp dom req resp) = ()
  type FwdSignalInner (ReqResp dom req resp) = Maybe req
  type BwdSignalInner (ReqResp dom req resp) = Maybe resp

  fwdFromSignal _ () s = s
  signalFromFwd _ () s = s
  bwdFromSignal _ () s = s
  signalFromBwd _ bwd = bwd

instance ProtocolSignals dom (Wishbone dom mode addressBits dataBytes) where
  type FwdConst (Wishbone dom mode addressBits dataBytes) = ()
  type BwdConst (Wishbone dom mode addressBits dataBytes) = ()
  type
    FwdSignalInner (Wishbone dom mode addressBits dataBytes) =
      WishboneM2S addressBits dataBytes
  type BwdSignalInner (Wishbone dom mode addressBits dataBytes) = WishboneS2M dataBytes

  fwdFromSignal _ () s = s
  signalFromFwd _ () s = s
  bwdFromSignal _ () s = s
  signalFromBwd _ bwd = bwd
