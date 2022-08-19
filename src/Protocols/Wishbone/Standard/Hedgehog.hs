{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiWayIf #-}

-- |
-- Types and functions to aid with testing Wishbone circuits.
--
-- This module provides two "modes" of Wishbone Specification compliance checks:
--
--  * "lenient" mode
--
--  * "common sense" mode
--
-- __\"lenient\" mode__
--
-- The Wishbone spec mostly specifies the handshake protocols but does not make many
-- assumptions about which signals *should* be valid in response to what. This is
-- presumably done so to allow circuits to be very flexible, however it makes for
-- a relatively weak validation tool.
--
-- __\"common sense\" mode__
--
-- The Wishbone spec itself makes very little assumptions about how interactions
-- should look like outside of basic hand-shaking.
-- This "common sense" compliance checking additionally checks for:
--  - A read request is acked with defined data
--    - response data respects the 'busSelect' signal
--  - A write request must contain valid data according to the 'busSelect' signal
module Protocols.Wishbone.Standard.Hedgehog
  ( WishboneMasterRequest (..),
    stallStandard,
    driveStandard,
    wishbonePropWithModel,
    validatorCircuit,
    validatorCircuitLenient
  )
where

import Clash.Prelude as C hiding (cycle, not, (&&), (||), indices)
import Clash.Signal.Internal (Signal ((:-)))
import qualified Data.Bifunctor as B
import Hedgehog ((===))
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Protocols hiding (stallC, circuit)
import Protocols.Hedgehog
import Protocols.Wishbone
import Prelude as P hiding (cycle)
import GHC.Stack (HasCallStack)

-- | Datatype representing a single transaction request sent from a Wishbone Master to a Wishbone Slave
data WishboneMasterRequest addressWidth dat
  = Read (BitVector addressWidth) (BitVector (BitSize dat `DivRU` 8))
  | Write (BitVector addressWidth) (BitVector (BitSize dat `DivRU` 8)) dat

deriving instance (KnownNat addressWidth, KnownNat (BitSize a), Show a) => (Show (WishboneMasterRequest addressWidth a))

--
-- Validation for (lenient) spec compliance
--

data LenientValidationState
  = LVSQuiet
  | LVSInCycleNoStrobe
  | LVSWaitForSlave
  deriving (Generic, NFDataX)


nextStateLenient ::
  LenientValidationState ->
  WishboneM2S addressWidth (BitSize a `DivRU` 8) a ->
  WishboneS2M a ->
  Either String LenientValidationState
nextStateLenient _ m2s s2m
  | P.length (filter ($ s2m) [acknowledge, err, retry]) > 1 =
    Left "More than one termination signal asserted"
  | not (busCycle m2s) && (acknowledge s2m || err s2m || retry s2m) =
    Left "Termination signals outside of a bus cycle"
nextStateLenient state m2s s2m = case state of
  LVSQuiet ->
    if
    | busCycle m2s && P.not (strobe m2s) -> Right LVSInCycleNoStrobe
    | busCycle m2s && strobe m2s -> Right LVSWaitForSlave
    | otherwise -> Right LVSQuiet
  LVSInCycleNoStrobe ->
    if
    | not (busCycle m2s) -> Right LVSQuiet
    | busCycle m2s && strobe m2s -> Right LVSWaitForSlave
    | otherwise -> Right LVSInCycleNoStrobe
  LVSWaitForSlave ->
    if
    | busCycle m2s && P.not (strobe m2s) -> Right LVSInCycleNoStrobe
    | not (busCycle m2s) -> Right LVSQuiet
    | acknowledge s2m || err s2m || retry s2m -> Right LVSQuiet
    | otherwise -> Right LVSWaitForSlave


--
-- Validation for "common sense" compliance
--

type CommonSenseValidationError = String

data CommonSenseValidationState
  = CSVSQuiet
  | CSVSInCycleNoStrobe
  | CSVSReadCycle
  | CSVSWriteCycle
  deriving (Eq, Show, Generic, NFDataX)

nextStateCommonSense ::
  forall a addressWidth.
  (BitPack a) =>
  CommonSenseValidationState ->
  WishboneM2S addressWidth (BitSize a `DivRU` 8) a ->
  WishboneS2M a ->
  Either CommonSenseValidationError (Bool, CommonSenseValidationState)
--                                    ^ go to next cycle
nextStateCommonSense _ _ s2m
  | P.length (filter ($ s2m) [acknowledge, err, retry]) > 1 =
    Left "More than one termination signal asserted"
nextStateCommonSense state m2s@WishboneM2S{..} s2m@WishboneS2M{..} = case state of
  CSVSQuiet ->
    if
    | busCycle && not strobe -> Right (True, CSVSInCycleNoStrobe)
    | busCycle && strobe && writeEnable -> Right (False, CSVSWriteCycle)
    | busCycle && strobe && not writeEnable -> Right (False, CSVSReadCycle)
    | hasTerminateFlag s2m -> Left "Termination signals outside of a bus cycle"
    | otherwise -> Right (True, CSVSQuiet)
  CSVSInCycleNoStrobe ->
    if
    | not busCycle -> Right (True, CSVSQuiet)
    | not strobe -> Right (True, CSVSInCycleNoStrobe)
    | writeEnable -> Right (False, CSVSWriteCycle)
    | not writeEnable -> Right (False, CSVSReadCycle)
    | otherwise -> C.error "Should not happen"
  CSVSReadCycle ->
    if
    | not busCycle -> Right (True, CSVSQuiet)
    | not strobe -> Right (True, CSVSInCycleNoStrobe)
    | acknowledge ->
      if responseValid m2s s2m
        then Right (True, CSVSQuiet)
        else Left "read-response does not respect SEL"
    | err || retry -> Right (True, CSVSQuiet)
    | writeEnable -> Left "asserted WE while in a read cycle"
    | otherwise -> Right (True, CSVSReadCycle)
  CSVSWriteCycle ->
    if
    | not busCycle -> Right (True, CSVSQuiet)
    | not strobe -> Right (True, CSVSInCycleNoStrobe)
    | not writeEnable -> Left "deasserted WE while in a write cycle"
    | not $ requestValid m2s -> Left "write request does not respect SEL"
    | err || retry -> Right (True, CSVSQuiet)
    | acknowledge -> Right (True, CSVSQuiet)
    | otherwise -> Right (True, CSVSWriteCycle)
 where
  responseValid WishboneM2S{busSelect=sel} WishboneS2M{readData=dat} = selectValidData sel dat
  requestValid WishboneM2S{busSelect=sel, writeData=dat} = selectValidData sel dat

-- | This function checks whether all bytes selected by \"SEL\" in a value
--   contain defined data.
selectValidData ::
  forall a.
  (HasCallStack, KnownNat (BitSize a), BitPack a) =>
  BitVector (BitSize a `DivRU` 8) ->
  a ->
  Bool
selectValidData byteSelect rawDat =
  all
    (== False)
    [ hasUndefined part
      | idx <- indices byteSelect,
        let part = dat ! idx
    ]
  where
    dat :: C.Vec (BitSize a `DivRU` 8) (BitVector 8)
    dat = case maybeIsX rawDat of
      Just val -> bitCoerce $ resize (bitCoerce val :: BitVector (BitSize a))
      Nothing -> C.deepErrorX "value to be 'select-checked' has an undefined spine"

    indices :: forall n. (KnownNat n) => BitVector n -> [Index n]
    indices bv = filter (testBit bv . fromIntegral) [0 .. maxBound]


-- | Create a stalling wishbone 'Standard' circuit.
stallStandard ::
  forall dom a addressWidth.
  ( C.ShowX a,
    Show a,
    C.NFDataX a,
    C.KnownNat addressWidth,
    C.KnownDomain dom,
    C.KnownNat (C.BitSize a)
  ) =>
  -- | Number of cycles to stall the master for on each valid bus-cycle
  [Int] ->
  Circuit
    (Wishbone dom 'Standard addressWidth a)
    (Wishbone dom 'Standard addressWidth a)
stallStandard stallsPerCycle =
  Circuit $
    B.second (emptyWishboneM2S :-)
      . uncurry (go stallsPerCycle Nothing)
  where
    go ::
      [Int] ->
      Maybe (WishboneS2M a) ->
      Signal dom (WishboneM2S addressWidth (BitSize a `DivRU` 8) a) ->
      Signal dom (WishboneS2M a) ->
      ( Signal dom (WishboneS2M a),
        Signal dom (WishboneM2S addressWidth (BitSize a `DivRU` 8) a)
      )

    go [] lastRep (_ :- m2s) ~(_ :- s2m) =
      B.bimap (emptyWishboneS2M :-) (emptyWishboneM2S :-) $ go [] lastRep m2s s2m
    go (st : stalls) lastRep (m :- m2s) ~(_ :- s2m)
      -- not in a bus cycle, just pass through
      | not (busCycle m) =
        B.bimap
          (emptyWishboneS2M :-)
          (emptyWishboneM2S :-)
          (go (st : stalls) lastRep m2s s2m)
    go (st : stalls) Nothing (m :- m2s) ~(s :- s2m)
      -- received a reply but still need to stall
      | busCycle m && strobe m && st > 0 && hasTerminateFlag s =
        B.bimap
          -- tell the master that the slave has no reply yet
          (emptyWishboneS2M :-)
          -- tell the slave that the cycle is over
          (emptyWishboneM2S :-)
          (go (st - 1 : stalls) (Just s) m2s s2m)
      -- received a reply but still need to stall
      | busCycle m && strobe m && st > 0 && not (hasTerminateFlag s) =
        B.bimap
          -- tell the master that the slave has no reply yet
          (emptyWishboneS2M :-)
          -- tell the slave that the cycle is over
          (m :-)
          (go (st - 1 : stalls) Nothing m2s s2m)
      -- done stalling, got a reply last second, pass through
      | busCycle m && strobe m && st == 0 && hasTerminateFlag s =
        B.bimap
          (s :-)
          (m :-)
          (go stalls Nothing m2s s2m)
      -- done stalling but no termination signal yet, just pass through to give the slave
      -- the chance to reply
      | busCycle m && strobe m && st == 0 && not (hasTerminateFlag s) =
        B.bimap
          (emptyWishboneS2M :-)
          (m :-)
          (go (0 : stalls) Nothing m2s s2m)
      -- master cancelled cycle
      | otherwise = B.bimap (emptyWishboneS2M :-) (m :-) (go stalls Nothing m2s s2m)
    go (st : stalls) (Just rep) (m :- m2s) ~(_ :- s2m)
      -- need to keep stalling, already got the reply
      | busCycle m && strobe m && st > 0 =
        B.bimap
          -- keep stalling
          (emptyWishboneS2M :-)
          -- tell the slave that the cycle is over
          (emptyWishboneM2S :-)
          (go (st - 1 : stalls) (Just rep) m2s s2m)
      -- done stalling, give reply
      | busCycle m && strobe m && st == 0 =
        B.bimap
          (rep :-)
          (emptyWishboneM2S :-)
          (go stalls Nothing m2s s2m)
      -- master cancelled cycle
      | otherwise = B.bimap (emptyWishboneS2M :-) (m :-) (go stalls Nothing m2s s2m)

-- | Create a wishbone 'Standard' circuit to drive other circuits.
driveStandard ::
  forall dom a addressWidth.
  ( C.ShowX a,
    Show a,
    C.NFDataX a,
    C.KnownNat addressWidth,
    C.KnownDomain dom,
    C.KnownNat (C.BitSize a)
  ) =>
  ExpectOptions ->
  -- | Requests to send out
  [WishboneMasterRequest addressWidth a] ->
  Circuit () (Wishbone dom 'Standard addressWidth a)
driveStandard ExpectOptions {..} reqs =
  Circuit $
    ((),)
      . C.fromList_lazy
      . (emptyWishboneM2S :)
      . go eoResetCycles reqs
      . C.sample_lazy
      . snd
  where
    transferToSignals ::
      forall b addrWidth.
      ( C.ShowX b,
        Show b,
        C.NFDataX b,
        C.KnownNat addrWidth,
        C.KnownNat (C.BitSize b)
      ) =>
      WishboneMasterRequest addrWidth b ->
      WishboneM2S addrWidth (BitSize b `DivRU` 8) b
    transferToSignals (Read addr sel) =
      (emptyWishboneM2S @addrWidth @b)
        { busCycle = True,
          strobe = True,
          addr = addr,
          busSelect = sel,
          writeEnable = False
        }
    transferToSignals (Write addr sel dat) =
      (emptyWishboneM2S @addrWidth @b)
        { busCycle = True,
          strobe = True,
          addr = addr,
          busSelect = sel,
          writeEnable = True,
          writeData = dat
        }

    go ::
      Int ->
      [WishboneMasterRequest addressWidth a] ->
      [WishboneS2M a] ->
      [WishboneM2S addressWidth (BitSize a `DivRU` 8) a]
    go nResets dat ~(rep : replies)
      | nResets > 0 = emptyWishboneM2S : (rep `C.seqX` go (nResets - 1) dat replies)
    -- no more data to send
    go _ [] ~(rep : replies) = emptyWishboneM2S : (rep `C.seqX` go 0 [] replies)
    go _ (d : dat) ~(rep : replies)
      -- the sent data was acknowledged, end the cycle before continuing
      | acknowledge rep || err rep = emptyWishboneM2S : (rep `C.seqX` go 0 dat replies)
      -- end cycle, continue but send the previous request again
      | retry rep = emptyWishboneM2S : (rep `C.seqX` go 0 (d : dat) replies)
      -- not a termination signal, so keep sending the data
      | otherwise -- trace "D in-cycle wait for ACK" $
        =
        transferToSignals d : (rep `C.seqX` go 0 (d : dat) replies)

-- | Circuit which validates the wishbone communication signals between a
--   master and a slave circuit.
--
--   Halts execution using 'error' when a "common sense" spec validation occurs.
--
--   N.B. Not synthesisable.
validatorCircuit ::
  forall dom addressWidth a.
  ( HasCallStack,
    HiddenClockResetEnable dom,
    KnownNat addressWidth,
    BitPack a,
    NFDataX a,
    ShowX a
  ) =>
  Circuit (Wishbone dom 'Standard addressWidth a) (Wishbone dom 'Standard addressWidth a)
validatorCircuit =
  Circuit $ mealyB go (0 :: Integer, (emptyWishboneM2S, emptyWishboneS2M), CSVSQuiet)
 where
  go (cycle, (m2s0, s2m0), state0) (m2s1, s2m1) =
    case nextStateCommonSense state0 m2s0 s2m0 of
      Left err ->
        error $
           "Wishbone common-sense validation error on cycle "
        <> show cycle
        <> ": " <> err
        <> "\n\n"
        <> "M2S: " <> show m2s0 <> "\n"
        <> "S2M: " <> show s2m0
      Right (True, state1) -> ((cycle + 1, (m2s1, s2m1), state1), (s2m1, m2s1))
      Right (False, state1) -> go (cycle, (m2s0, s2m0), state1) (m2s1, s2m1)


-- | Circuit which validates the wishbone communication signals between a
--   master and a slave circuit.
--
--   Halts execution using 'error' when a spec validation occurs.
--
--   N.B. Not synthesisable.
validatorCircuitLenient ::
  forall dom addressWidth a.
  ( HasCallStack,
    HiddenClockResetEnable dom,
    KnownNat addressWidth,
    BitPack a,
    NFDataX a,
    ShowX a
  ) =>
  Circuit (Wishbone dom 'Standard addressWidth a) (Wishbone dom 'Standard addressWidth a)
validatorCircuitLenient =
  Circuit $ mealyB go (0 :: Integer, (emptyWishboneM2S, emptyWishboneS2M), LVSQuiet)
 where
  go (cycle, (m2s0, s2m0), state0) (m2s1, s2m1) =
    case nextStateLenient state0 m2s0 s2m0 of
      Left err ->
        error $
           "Wishbone lenient validation error on cycle "
        <> show cycle
        <> ": " <> err
        <> "\n\n"
        <> "M2S: " <> show m2s0 <> "\n"
        <> "S2M: " <> show s2m0
      Right state1 -> ((cycle + 1, (m2s1, s2m1), state1), (s2m1, m2s1))

-- | Test a wishbone 'Standard' circuit against a pure model.
wishbonePropWithModel ::
  forall dom a addressWidth st.
  ( Eq a,
    C.ShowX a,
    Show a,
    C.NFDataX a,
    C.KnownNat addressWidth,
    C.HiddenClockResetEnable dom,
    C.KnownNat (C.BitSize a),
    C.BitPack a
  ) =>
  ExpectOptions ->
  -- | Check whether a S2M signal for a given request is matching a pure model using @st@
  --   as its state.
  --   Return an error message 'Left' or the updated state 'Right'
  (WishboneMasterRequest addressWidth a -> WishboneS2M a -> st -> Either String st) ->
  -- | The circuit to run the test against.
  Circuit (Wishbone dom 'Standard addressWidth a) () ->
  -- | Inputs to the circuit and model
  H.Gen [WishboneMasterRequest addressWidth a] ->
  -- | Initial state of the model
  st ->
  H.Property
wishbonePropWithModel eOpts model circuit0 inputGen st = H.property $ do
  input <- H.forAll inputGen

  let
    n = P.length input
    genStall = Gen.integral (Range.linear 0 10)

  stalls <- H.forAll (Gen.list (Range.singleton n) genStall)

  let
    resets = 50
    driver = driveStandard @dom (defExpectOptions {eoResetCycles = resets}) input
    stallC = stallStandard stalls
    circuit1 = stallC |> validatorCircuit |> circuit0
    (_, _ : s2m) = observeComposedWishboneCircuit (eoTimeout eOpts) driver circuit1

  matchModel 0 s2m input st === Right ()
  where
    matchModel ::
      Int ->
      [WishboneS2M a] ->
      [WishboneMasterRequest addressWidth a] ->
      st ->
      Either (Int, String) ()
    matchModel _ [] _ _ = Right () -- so far everything is good but the sampling stopped
    matchModel _ _ [] _ = Right ()
    matchModel cyc (s : s2m) (req : reqs0) state
      | not (hasTerminateFlag s) = s `C.seqX` matchModel (succ cyc) s2m (req : reqs0) state
      | otherwise = case model req s state of
        Left err -> Left (cyc, err)
        Right st1 -> s `C.seqX` matchModel (succ cyc) s2m reqs1 st1
          where
            reqs1
              | retry s = req : reqs0
              | otherwise = reqs0

observeComposedWishboneCircuit ::
  (KnownDomain dom) =>
  Maybe Int ->
  Circuit () (Wishbone dom mode addressWidth a) ->
  Circuit (Wishbone dom mode addressWidth a) () ->
  ( [WishboneM2S addressWidth (BitSize a `DivRU` 8) a],
    [WishboneS2M a]
  )
observeComposedWishboneCircuit Nothing (Circuit master) (Circuit slave) =
  let ~((), m2s) = master ((), s2m)
      ~(s2m, ()) = slave (m2s, ())
   in (sample_lazy m2s, sample_lazy s2m)
observeComposedWishboneCircuit (Just n) (Circuit master) (Circuit slave) =
  let ~((), m2s) = master ((), s2m)
      ~(s2m, ()) = slave (m2s, ())
   in (sampleN_lazy n m2s, sampleN_lazy n s2m)
