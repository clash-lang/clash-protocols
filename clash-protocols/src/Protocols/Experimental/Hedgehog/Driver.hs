module Protocols.Experimental.Hedgehog.Driver (
  ProtocolSignals (..),
  CircuitDriverInstructions,
  CircuitDriverT,
  CircuitDriver,
  runCircuitDriverT,
  runCircuitDriver,
  step,
  circuitLhsToSignals,
) where

import Clash.Prelude hiding (fromSignal, singleton)
import Clash.Signal.Internal (Signal ((:-)))
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

The `dom` parameter is needed because not all 'Protocol's represent their
'Fwd' and 'Bwd' channels with signals.
-}
class (Protocol p) => ProtocolSignals dom p where
  type FwdSignalInner p
  type BwdSignalInner p

  fwdFromSignal :: Proxy p -> Signal dom (FwdSignalInner p) -> Fwd p
  signalFromBwd :: (Proxy p, Proxy dom) -> Bwd p -> Signal dom (BwdSignalInner p)

{- | The "instructions" for a driver. These are the building blocks
for monadic operations.
-}
data CircuitDriverInstructions dom p res where
  Step :: FwdSignalInner p -> CircuitDriverInstructions dom p (BwdSignalInner p)

-- Circuit driver program in the monad `m`, computing a `res` value.
type CircuitDriverT dom p m res = ProgramT (CircuitDriverInstructions dom p) m res

-- A non-monadic circuit driver program, computing a `res` value.
type CircuitDriver dom p res = Program (CircuitDriverInstructions dom p) res

circuitLhsToSignals ::
  forall dom a.
  (ProtocolSignals dom a) =>
  Circuit a () ->
  Signal dom (FwdSignalInner a) ->
  Signal dom (BwdSignalInner a)
circuitLhsToSignals (Circuit fn) fwdInnerSig =
  signalFromBwd (Proxy @a, Proxy @dom) $
    fwdBwdFn (fwdFromSignal (Proxy @a) fwdInnerSig)
 where
  fwdBwdFn = \fwd -> fst (fn (fwd, ()))

-- | Run a circuit driver in the 'Monad' `m`.
runCircuitDriverT ::
  forall dom a m res.
  (Monad m) =>
  (KnownDomain dom, ProtocolSignals dom a) =>
  -- | Circuit to be simulated/driven
  Circuit a () ->
  -- | driver "program" to run
  CircuitDriverT dom a m res ->
  m res
runCircuitDriverT circ prog = run (signalAutomaton @dom $ circuitLhsToSignals circ) prog
 where
  run ::
    Automaton (->) (FwdSignalInner a) (BwdSignalInner a) ->
    CircuitDriverT dom a m res ->
    m res
  run (Automaton step0) driver = do
    driverView <- viewT driver
    case driverView of
      Return val -> pure val
      Step fwd :>>= next -> do
        let (bwd, nextAutomaton) = step0 fwd
        run nextAutomaton (next bwd)

-- | Run a pure circuit driver.
runCircuitDriver ::
  forall dom a res.
  (KnownDomain dom, ProtocolSignals dom a) =>
  Circuit a () ->
  CircuitDriver dom a res ->
  res
runCircuitDriver circ prog = runIdentity $ runCircuitDriverT circ prog

{- | Perform a simulation step of the circuit.

This will provide `fwd` as the input and will give the `bwd` value
as the monadic binder.
-}
step :: FwdSignalInner p -> CircuitDriverT dom p m (BwdSignalInner p)
step fwd = singleton $ Step fwd

--
-- Some instances for `ProtocolSignals`
--

instance ProtocolSignals dom () where
  type FwdSignalInner () = ()
  type BwdSignalInner () = ()

  fwdFromSignal _ _ = ()
  signalFromBwd _ () = pure ()

instance ProtocolSignals dom (ToConst a) where
  type FwdSignalInner (ToConst a) = a
  type BwdSignalInner (ToConst a) = ()

  fwdFromSignal _ (a :- _) = a
  signalFromBwd _ () = pure ()

instance ProtocolSignals dom (CSignal dom a) where
  type FwdSignalInner (CSignal dom a) = a
  type BwdSignalInner (CSignal dom a) = ()

  fwdFromSignal _ signal = signal
  signalFromBwd _ () = pure ()

instance (KnownNat n, ProtocolSignals dom p) => ProtocolSignals dom (Vec n p) where
  type FwdSignalInner (Vec n p) = Vec n (FwdSignalInner p)
  type BwdSignalInner (Vec n p) = Vec n (BwdSignalInner p)

  fwdFromSignal _ fwds = fwdFromSignal (Proxy @p) <$> unbundle fwds
  signalFromBwd _ bwds = bundle $ signalFromBwd (Proxy @p, Proxy @dom) <$> bwds

instance (ProtocolSignals dom a, ProtocolSignals dom b) => ProtocolSignals dom (a, b) where
  type FwdSignalInner (a, b) = (FwdSignalInner a, FwdSignalInner b)
  type BwdSignalInner (a, b) = (BwdSignalInner a, BwdSignalInner b)

  fwdFromSignal _ (unbundle -> (fwdA, fwdB)) = (fwdFromSignal (Proxy @a) fwdA, fwdFromSignal (Proxy @b) fwdB)
  signalFromBwd _ (bwdA, bwdB) =
    bundle $
      (signalFromBwd (Proxy @a, Proxy @dom) bwdA, signalFromBwd (Proxy @b, Proxy @dom) bwdB)

instance ProtocolSignals dom (Df dom a) where
  type FwdSignalInner (Df dom a) = Maybe a
  type BwdSignalInner (Df dom a) = Ack

  fwdFromSignal _ s = s
  signalFromBwd _ bwd = bwd

instance ProtocolSignals dom (ReqResp dom req resp) where
  type FwdSignalInner (ReqResp dom req resp) = Maybe req
  type BwdSignalInner (ReqResp dom req resp) = Maybe resp

  fwdFromSignal _ s = s
  signalFromBwd _ bwd = bwd

instance ProtocolSignals dom (Wishbone dom mode addressBits dataBytes) where
  type
    FwdSignalInner (Wishbone dom mode addressBits dataBytes) =
      WishboneM2S addressBits dataBytes
  type BwdSignalInner (Wishbone dom mode addressBits dataBytes) = WishboneS2M dataBytes

  fwdFromSignal _ s = s
  signalFromBwd _ bwd = bwd
