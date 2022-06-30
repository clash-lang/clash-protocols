{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Protocols.Wishbone.Hedgehog where

import           Clash.Prelude         as C hiding (cycle, not, (&&), (||))
import           Clash.Signal.Internal (Signal ((:-)))
import           Prelude               as P hiding (cycle)

-- hedgehog
import           Control.DeepSeq       (NFData)
import qualified Hedgehog              as H
import qualified Hedgehog.Gen          as Gen
import qualified Hedgehog.Range        as Range


-- me
import qualified Data.Bifunctor        as B
import           Data.Maybe            (fromMaybe)
import           Debug.Trace
import           Hedgehog              ((===))
import           Protocols
import           Protocols.Hedgehog
import           Protocols.Wishbone




data WishboneMasterRequest addressWidth dat
  = Read (BitVector addressWidth)
  | Write (BitVector addressWidth) dat
  deriving (Show)


data WishboneStandardState
  = WSSQuiet
  | WSSInCycleNoStrobe
  | WSSWaitForSlave
  -- TODO add a SlaveHolding state? Spec seems unclear

data WishboneStandardError
  = WSEMoreThanOneTerminationSignalAsserted
  | WSETerminationSignalOutsideOfCycle
  deriving (NFData, Generic, NFDataX, Show, ShowX, Eq)


nextStateStandard
  :: WishboneStandardState
  -> WishboneM2S addressWidth (BitSize a `DivRU` 8) a
  -> WishboneS2M a
  -> Either WishboneStandardError WishboneStandardState
nextStateStandard _ m2s s2m
  | P.length (filter ($ s2m) [acknowledge, err, retry]) > 1         = Left WSEMoreThanOneTerminationSignalAsserted
  | not (busCycle m2s) && (acknowledge s2m || err s2m || retry s2m) = Left WSETerminationSignalOutsideOfCycle
nextStateStandard WSSQuiet m2s _
  | busCycle m2s && P.not (strobe m2s)      = Right WSSInCycleNoStrobe
  | busCycle m2s && strobe m2s              = Right WSSWaitForSlave
  | otherwise                               = Right WSSQuiet
nextStateStandard WSSInCycleNoStrobe m2s _
  | not (busCycle m2s)         = Right WSSQuiet
  | busCycle m2s && strobe m2s = Right WSSWaitForSlave
  | otherwise                  = Right WSSInCycleNoStrobe
nextStateStandard WSSWaitForSlave m2s s2m
  | busCycle m2s && P.not (strobe m2s)      = Right WSSInCycleNoStrobe
  | not (busCycle m2s)                      = Right WSSQuiet
  | acknowledge s2m || err s2m || retry s2m = Right WSSQuiet
  | otherwise                               = Right WSSWaitForSlave


-- | Validate the input/output streams of a standard wishbone interface
--
-- Returns 'Right ()' when the interactions comply with the spec.
-- Returns 'Left (cycle, err)' when a spec violation was found.
validateStandard :: [WishboneM2S addressWidth (BitSize a `DivRU` 8) a] -> [WishboneS2M a] -> Either (Int, WishboneStandardError) ()
validateStandard m2s s2m = go 0 (P.zip m2s s2m) WSSQuiet
  where
    go _ [] _                 = Right ()
    go n ((fwd, bwd):rest) st = case nextStateStandard st fwd bwd of
      Left e    -> Left (n, e)
      Right st' -> go (n + 1) rest st'


stallStandard
  :: forall dom a addressWidth
  . (C.ShowX a, Show a, C.NFDataX a, C.KnownNat addressWidth, C.KnownDomain dom, C.KnownNat (C.BitSize a))
  => [Int] -- number of cycles for a slave to stall
  -> Circuit (Wishbone dom 'Standard addressWidth a) (Wishbone dom 'Standard addressWidth a)
stallStandard stalls = Circuit $ B.second (wishboneM2S :-) . uncurry (go stalls Nothing)
  where
    go
      :: [Int]
      -> Maybe (WishboneS2M a)
      -> Signal dom (WishboneM2S addressWidth (BitSize a `DivRU` 8) a)
      -> Signal dom (WishboneS2M a)
      -> ( Signal dom (WishboneS2M a)
         , Signal dom (WishboneM2S addressWidth (BitSize a `DivRU` 8) a))

    go [] lastRep (_:-m2s) ~(_:-s2m) = B.bimap (wishboneS2M :-) (wishboneM2S :-) $ go [] lastRep m2s s2m

    go (st:stalls) lastRep (m:-m2s) ~(_:-s2m)
      -- not in a bus cycle, just pass through
      | not (busCycle m) = B.bimap (wishboneS2M:-) (wishboneM2S:-) $ go (st:stalls) lastRep m2s s2m
    go (st:stalls) Nothing (m:-m2s) ~(s:-s2m)
      -- received a reply but still need to stall
      | busCycle m && strobe m && st > 0 && terminationSignal s =
          B.bimap
            -- tell the master that the slave has no reply yet
            (wishboneS2M :-)
            -- tell the slave that the cycle is over
            (wishboneM2S :- )
            (go (st - 1:stalls) (Just s) m2s s2m)

      -- received a reply but still need to stall
      | busCycle m && strobe m && st > 0 && not (terminationSignal s) =
          B.bimap
            -- tell the master that the slave has no reply yet
            (wishboneS2M :-)
            -- tell the slave that the cycle is over
            (m :- )
            (go (st - 1:stalls) Nothing m2s s2m)

      -- done stalling, got a reply last second, pass through
      | busCycle m && strobe m && st == 0 && terminationSignal s =
          B.bimap
            (s :-)
            (m :-)
            (go stalls Nothing m2s s2m)
      -- done stalling but no termination signal yet, just pass through to give the slave
      -- the chance to reply
      | busCycle m && strobe m && st == 0 && not (terminationSignal s) =
          B.bimap
            (wishboneS2M :-)
            (m :-)
            (go (0:stalls) Nothing m2s s2m)
      -- master cancelled cycle
      | otherwise = trace "S master cancelled cycle (no reply)" $ B.bimap (wishboneS2M :-) (m :-) (go stalls Nothing m2s s2m)
    go (st:stalls) (Just rep) (m:-m2s) ~(_:-s2m)
      -- need to keep stalling, already got the reply
      | busCycle m && strobe m && st > 0 =
          B.bimap
            -- keep stalling
            (wishboneS2M :-)
            -- tell the slave that the cycle is over
            (wishboneM2S :-)
            (go (st - 1:stalls) (Just rep) m2s s2m)
      -- done stalling, give reply
      | busCycle m && strobe m && st == 0 =
          B.bimap
            (rep :-)
            (wishboneM2S :-)
            (go stalls Nothing m2s s2m)
      -- master cancelled cycle
      | otherwise = trace "S master cancelled cycle (got reply)" $ B.bimap (wishboneS2M :-) (m :-) (go stalls Nothing m2s s2m)


driveStandard
  :: forall dom a addressWidth
  . (C.ShowX a, Show a, C.NFDataX a, C.KnownNat addressWidth, C.KnownDomain dom, C.KnownNat (C.BitSize a))
  => ExpectOptions
  -> [WishboneMasterRequest addressWidth a]
  -> Circuit () (Wishbone dom 'Standard addressWidth a)
driveStandard ExpectOptions{..} dat = Circuit $ ((),) . C.fromList_lazy . (wishboneM2S:) . go eoResetCycles dat . C.sample_lazy . snd
  where

    transferToSignals
      :: forall a addressWidth
      . (C.ShowX a, Show a, C.NFDataX a, C.KnownNat addressWidth, C.KnownNat (C.BitSize a))
      => WishboneMasterRequest addressWidth a
      -> WishboneM2S addressWidth (BitSize a `DivRU` 8) a
    transferToSignals (Read addr) = (wishboneM2S @addressWidth @a) { busCycle = True, strobe = True, addr = addr, writeEnable = False }
    transferToSignals (Write addr dat) = (wishboneM2S @addressWidth @a) { busCycle = True, strobe = True, addr = addr, writeEnable = True, writeData = dat }

    go
      :: Int
      -> [WishboneMasterRequest addressWidth a]
      -> [WishboneS2M a]
      -> [WishboneM2S addressWidth (BitSize a `DivRU` 8) a]
    go nResets dat ~(rep:replies)
      | nResets > 0 = wishboneM2S : (rep `C.seqX` go (nResets - 1) dat replies)

    -- no more data to send
    go _ [] ~(rep:replies) = wishboneM2S : (rep `C.seqX` go 0 [] replies)

    go _ (d:dat) ~(rep:replies)
      -- the sent data was acknowledged, end the cycle before continuing
      | acknowledge rep || err rep = wishboneM2S : (rep `C.seqX` go 0 dat replies)
      -- end cycle, continue but send the previous request again
      | retry rep = wishboneM2S : (rep `C.seqX` go 0 (d:dat) replies)
      -- not a termination signal, so keep sending the data
      | otherwise = -- trace "D in-cycle wait for ACK" $
          transferToSignals d : (rep `C.seqX` go 0 (d:dat) replies)


validateStandardCircuit
  :: forall dom a addressWidth
  . (C.ShowX a, Show a, C.KnownNat addressWidth, C.KnownDomain dom, C.KnownNat (C.BitSize a))
  => Int
  -> Circuit () (Wishbone dom 'Standard addressWidth a)
  -> Circuit (Wishbone dom 'Standard addressWidth a) ()
  -> Either (Int, WishboneStandardError) ()
validateStandardCircuit cycles master slave =
  let (m2s, _:s2m) = observeComposedWishboneCircuit (Just cycles) master slave
  in validateStandard m2s s2m


validateStallingStandardCircuitInner
  :: forall dom a addressWidth
  . (C.ShowX a, Show a, C.NFDataX a, C.KnownNat addressWidth, C.KnownDomain dom, C.KnownNat (C.BitSize a))
  => ExpectOptions
  -> [WishboneMasterRequest addressWidth a]
  -> [Int]
  -> Circuit (Wishbone dom 'Standard addressWidth a) ()
  -> Either (Int, WishboneStandardError) ()
validateStallingStandardCircuitInner eOpts reqs stalls slave =
  let
    driveCirc = driveStandard eOpts reqs
    lhsStallC = stallStandard stalls
    stalledProtocol = lhsStallC |>
      slave
  in

  validateStandardCircuit (fromMaybe 50 $ eoTimeout eOpts) driveCirc stalledProtocol

validateStallingStandardCircuit
  :: forall dom a addressWidth
  . (C.ShowX a, Show a, C.NFDataX a, C.KnownNat addressWidth, C.KnownDomain dom, C.KnownNat (C.BitSize a))
  => ExpectOptions
  -> H.Gen [WishboneMasterRequest addressWidth a]
  -> Circuit (Wishbone dom 'Standard addressWidth a) ()
  -> H.Property
validateStallingStandardCircuit eOpts gen circ = H.property $ do
  dat <- H.forAll gen
  let n = P.length dat

  -- TODO: Different distributions?
  let genStall = Gen.integral (Range.linear 0 10)

  -- Generate stalls for LHS part of the protocol. The first line determines
  -- whether to stall or not. The second determines how many cycles to stall
  -- on each _valid_ cycle.
  stalls <- H.forAll (Gen.list (Range.singleton n) genStall)

  validateStallingStandardCircuitInner eOpts dat stalls circ === Right ()



wishbonePropWithModelInner
  :: forall dom a addressWidth st
  . (Eq a, C.ShowX a, Show a, C.NFDataX a, C.KnownNat addressWidth, C.KnownDomain dom, C.KnownNat (C.BitSize a))
  => (WishboneMasterRequest addressWidth a -> WishboneS2M a -> st -> Either String st)
  -- ^ Check whether a S2M signal for a given request is matching a pure model using 'st' as its
  --   state.
  -> Circuit (Wishbone dom 'Standard addressWidth a) ()
  -> [Int]
  -- ^ Stalls
  -> [WishboneMasterRequest addressWidth a]
  -- ^ Inputs to the circuit and model
  -> st
  -> Either (Int, String) ()
wishbonePropWithModelInner model circuit stalls input st =
    let
      resets = 50
      driver = driveStandard @dom (defExpectOptions { eoResetCycles = resets }) input
      stallC = stallStandard stalls
      circuit' = stallC |> circuit
      (_, _:s2m) = observeComposedWishboneCircuit Nothing driver circuit'
    in matchModel 0 s2m input st

  where
    matchModel
      :: Int
      -> [WishboneS2M a]
      -> [WishboneMasterRequest addressWidth a]
      -> st
      -> Either (Int, String) ()
    matchModel _ [] _  _ = C.error "S2M signal should never be empty"
    matchModel _ _  [] _ = Right ()
    matchModel cyc (s:s2m) (req:reqs) st
      | not (terminationSignal s) = s `C.seqX` matchModel (succ cyc) s2m (req:reqs) st
      | otherwise                 = case model req s st of
          Left err -> Left (cyc, err)
          Right st' -> s `C.seqX` matchModel (succ cyc) s2m reqs' st'
            where
              reqs'
                | retry s   = req:reqs
                | otherwise = reqs


wishbonePropWithModel
  :: forall dom a addressWidth st
  . (Eq a, C.ShowX a, Show a, C.NFDataX a, C.KnownNat addressWidth, C.KnownDomain dom, C.KnownNat (C.BitSize a))
  => (WishboneMasterRequest addressWidth a -> WishboneS2M a -> st -> Either String st)
  -- ^ Check whether a S2M signal for a given request is matching a pure model using 'st' as its
  --   state.
  -> Circuit (Wishbone dom 'Standard addressWidth a) ()
  -> H.Gen [WishboneMasterRequest addressWidth a]
  -- ^ Inputs to the circuit and model
  -> st
  -> H.Property
wishbonePropWithModel model circuit inputGen st = H.property $ do
    input <- H.forAll inputGen

    let n = P.length input
    let genStall = Gen.integral (Range.linear 0 10)

    stalls <- H.forAll (Gen.list (Range.singleton n) genStall)

    wishbonePropWithModelInner model circuit stalls input st === Right ()









observeComposedWishboneCircuit
  :: (KnownDomain dom)
  => Maybe Int
  -> Circuit () (Wishbone dom mode addressWidth a)
  -> Circuit (Wishbone dom mode addressWidth a) ()
  -> (
      [WishboneM2S addressWidth (BitSize a `DivRU` 8) a],
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
