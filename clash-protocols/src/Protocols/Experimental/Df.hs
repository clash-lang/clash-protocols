{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{- |
Experimental simulation support for "Protocols.Df".
-}
module Protocols.Experimental.Df (
  module Protocols.Df,
  module Protocols.Experimental.Simulate,

  -- * Simulation functions
  drive,
  stall,
  sample,
  simulate,
) where

import Clash.Explicit.Prelude qualified as CE
import Clash.Prelude qualified as C
import Clash.Signal.Internal (Signal (..))
import Data.Bifunctor qualified as B
import Data.Bool (bool)
import Data.Coerce qualified as Coerce
import Data.List ((\\))
import Data.Maybe qualified as Maybe
import Data.Proxy
import GHC.Stack (HasCallStack)
import Prelude
import Prelude qualified as P

import Protocols
import Protocols.Df
import Protocols.Experimental.Simulate

{- $setup
>>> import Protocols
>>> import Protocols.Experimental.Df
-}

instance Backpressure (Df dom a) where
  boolsToBwd _ = C.fromList_lazy . Coerce.coerce

instance (C.KnownDomain dom, C.NFDataX a, C.ShowX a, Show a) => Simulate (Df dom a) where
  type SimulateFwdType (Df dom a) = [Maybe a]
  type SimulateBwdType (Df dom a) = [Ack]
  type SimulateChannels (Df dom a) = 1

  simToSigFwd _ = C.fromList_lazy
  simToSigBwd _ = C.fromList_lazy
  sigToSimFwd _ s = C.sample_lazy s
  sigToSimBwd _ s = C.sample_lazy s

  stallC conf (C.head -> (stallAck, stalls)) = stall conf stallAck stalls

instance (C.KnownDomain dom, C.NFDataX a, C.ShowX a, Show a) => Drivable (Df dom a) where
  type ExpectType (Df dom a) = [a]

  toSimulateType Proxy = P.map Just
  fromSimulateType Proxy = Maybe.catMaybes

  driveC = drive
  sampleC = sample

{- | Emit values given in list. Emits no data while reset is asserted. Not
synthesizable.
-}
drive ::
  forall dom a.
  (C.KnownDomain dom) =>
  SimulationConfig ->
  [Maybe a] ->
  Circuit () (Df dom a)
drive SimulationConfig{resetCycles} s0 =
  Circuit $
    ((),)
      . C.fromList_lazy
      . go s0 resetCycles
      . CE.sample_lazy
      . P.snd
 where
  go _ resetN ~(ack : acks)
    | resetN > 0 =
        Nothing : (ack `C.seqX` go s0 (resetN - 1) acks)
  go [] _ ~(ack : acks) =
    Nothing : (ack `C.seqX` go [] 0 acks)
  go (Nothing : is) _ ~(ack : acks) =
    Nothing : (ack `C.seqX` go is 0 acks)
  go (Just dat : is) _ ~(Ack ack : acks) =
    Just dat : go (if ack then is else Just dat : is) 0 acks

{- | Sample protocol to a list of values. Drops values while reset is asserted.
Not synthesizable.

For a generalized version of 'sample', check out 'sampleC'.
-}
sample ::
  forall dom b.
  (C.KnownDomain dom) =>
  SimulationConfig ->
  Circuit () (Df dom b) ->
  [Maybe b]
sample SimulationConfig{..} c =
  CE.sampleN_lazy timeoutAfter $
    ignoreWhileInReset $
      P.snd $
        toSignals c ((), Ack <$> rst_n)
 where
  ignoreWhileInReset s =
    uncurry (bool Nothing)
      <$> C.bundle (s, rst_n)

  rst_n = C.fromList (replicate resetCycles False <> repeat True)

{- | Stall every valid Df packet with a given number of cycles. If there are
more valid packets than given numbers, passthrough all valid packets without
stalling. Not synthesizable.

For a generalized version of 'stall', check out 'stallC'.
-}
stall ::
  forall dom a.
  ( C.KnownDomain dom
  , HasCallStack
  ) =>
  SimulationConfig ->
  {- | Acknowledgement to send when LHS does not send data. Stall will act
  transparently when reset is asserted.
  -}
  StallAck ->
  -- Number of cycles to stall for every valid Df packet
  [Int] ->
  Circuit (Df dom a) (Df dom a)
stall SimulationConfig{..} stallAck stalls =
  Circuit $
    uncurry (go stallAcks stalls resetCycles)
 where
  stallAcks
    | stallAck == StallCycle = [minBound .. maxBound] \\ [StallCycle]
    | otherwise = [stallAck]

  toStallAck :: Maybe a -> Ack -> StallAck -> Ack
  toStallAck (Just _) ack = P.const ack
  toStallAck Nothing ack = \case
    StallWithNack -> Ack False
    StallWithAck -> Ack True
    StallWithErrorX -> C.errorX "No defined ack"
    StallTransparently -> ack
    StallCycle -> Ack False -- shouldn't happen..
  go ::
    [StallAck] ->
    [Int] ->
    Int ->
    Signal dom (Maybe a) ->
    Signal dom Ack ->
    ( Signal dom Ack
    , Signal dom (Maybe a)
    )
  go [] ss rs fwd bwd =
    go stallAcks ss rs fwd bwd
  go (_ : sas) _ resetN (f :- fwd) ~(b :- bwd)
    | resetN > 0 =
        B.bimap (b :-) (f :-) (go sas stalls (resetN - 1) fwd bwd)
  go (sa : sas) [] _ (f :- fwd) ~(b :- bwd) =
    B.bimap (toStallAck f b sa :-) (f :-) (go sas [] 0 fwd bwd)
  go (sa : sas) ss _ (Nothing :- fwd) ~(b :- bwd) =
    -- Left hand side does not send data, simply replicate that behavior. Right
    -- hand side might send an arbitrary acknowledgement, so we simply pass it
    -- through.
    B.bimap (toStallAck Nothing b sa :-) (Nothing :-) (go sas ss 0 fwd bwd)
  go (_sa : sas) (s : ss) _ (f0 :- fwd) ~(Ack b0 :- bwd) =
    let
      -- Stall as long as s > 0. If s ~ 0, we wait for the RHS to acknowledge
      -- the data. As long as RHS does not acknowledge the data, we keep sending
      -- the same data.
      (f1, b1, s1) = case compare 0 s of
        LT -> (Nothing, Ack False, pred s : ss) -- s > 0
        EQ -> (f0, Ack b0, if b0 then ss else s : ss) -- s ~ 0
        GT -> error ("Unexpected negative stall: " <> show s) -- s < 0
     in
      B.bimap (b1 :-) (f1 :-) (go sas s1 0 fwd bwd)

{- | Simulate a single domain protocol. Not synthesizable.

For a generalized version of 'simulate', check out
'Protocols.Experimental.Simulate.simulateC'.
-}
simulate ::
  forall dom a b.
  (C.KnownDomain dom) =>
  -- | Simulation configuration. Use 'Data.Default.def' for sensible defaults.
  SimulationConfig ->
  -- | Circuit to simulate.
  ( C.Clock dom ->
    C.Reset dom ->
    C.Enable dom ->
    Circuit (Df dom a) (Df dom b)
  ) ->
  -- | Inputs
  [Maybe a] ->
  -- | Outputs
  [Maybe b]
simulate conf@SimulationConfig{..} circ inputs =
  sample conf (drive conf inputs |> circ clk rst ena)
 where
  (clk, rst, ena) = (C.clockGen, resetGen resetCycles, C.enableGen)

{- | Like 'C.resetGenN', but works on 'Int' instead of 'C.SNat'. Not
synthesizable.
-}
resetGen :: (C.KnownDomain dom) => Int -> C.Reset dom
resetGen n =
  C.unsafeFromActiveHigh $
    C.fromList (replicate n True <> repeat False)
