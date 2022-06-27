{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RecordWildCards  #-}
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
  => SimulationConfig
  -> [Int] -- number of cycles for a slave to stall
  -> Circuit (Wishbone dom 'Standard addressWidth a) (Wishbone dom 'Standard addressWidth a)
stallStandard config stalls = Circuit $ B.second (wishboneM2S :-) . uncurry (go (resetCycles config) stalls Nothing)
  where
    go
      :: Int
      -> [Int]
      -> Maybe (WishboneS2M a)
      -> Signal dom (WishboneM2S addressWidth (BitSize a `DivRU` 8) a)
      -> Signal dom (WishboneS2M a)
      -> ( Signal dom (WishboneS2M a)
         , Signal dom (WishboneM2S addressWidth (BitSize a `DivRU` 8) a))
    go resets stalls lastRep (m:-m2s) ~(s:-s2m)
      | resets > 0 = B.bimap (s:-) (m:-) $ go (resets - 1) stalls lastRep m2s s2m

    go _ [] lastRep (m:-m2s) ~(s:-s2m) = B.bimap (wishboneS2M :-) (wishboneM2S :-) $ go 0 [] lastRep m2s s2m

    go _ (st:stalls) lastRep (m:-m2s) ~(s:-s2m)
      -- not in a bus cycle, just pass through
      | not (busCycle m) = B.bimap (s:-) (m:-) $ go 0 (st:stalls) lastRep m2s s2m
    go _ (st:stalls) Nothing (m:-m2s) ~(s:-s2m)
      -- received a reply but still need to stall
      | busCycle m && strobe m && st > 0 && terminationSignal s =
          B.bimap
            -- tell the master that the slave has no reply yet
            (wishboneS2M :-)
            -- tell the slave that the cycle is over
            (wishboneM2S :- )
            (go 0 (st - 1:stalls) (Just s) m2s s2m)

      -- done stalling, got a reply last second, pass through
      | busCycle m && strobe m && st == 0 && terminationSignal s =
          B.bimap
            (s :-)
            (m :-)
            (go 0 stalls Nothing m2s s2m)
      -- done stalling but no termination signal yet, just pass through to give the slave
      -- the chance to reply
      | busCycle m && strobe m && st == 0 && not (terminationSignal s) =
          B.bimap
            (s :-)
            (m :-)
            (go 0 (0:stalls) Nothing m2s s2m)
      -- master cancelled cycle
      | otherwise = B.bimap (wishboneS2M :-) (m :-) (go 0 stalls Nothing m2s s2m)
    go _ (st:stalls) (Just rep) (m:-m2s) ~(s:-s2m)
      -- need to keep stalling, already got the reply
      | busCycle m && strobe m && st > 0 =
          B.bimap
            -- keep stalling
            (wishboneS2M :-)
            -- tell the slave that the cycle is over
            (wishboneM2S :-)
            (go 0 (st - 1:stalls) (Just rep) m2s s2m)
      -- done stalling, give reply
      | busCycle m && strobe m && st == 0 =
          B.bimap
            (rep :-)
            (wishboneM2S :-)
            (go 0 stalls Nothing m2s s2m)
      -- master cancelled cycle
      | otherwise = B.bimap (wishboneS2M :-) (m :-) (go 0 stalls Nothing m2s s2m)


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
      | otherwise = transferToSignals d : (rep `C.seqX` go 0 (d:dat) replies)


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
    simConfig = def {resetCycles = eoResetCycles eOpts}
    driveCirc = driveStandard eOpts reqs
    lhsStallC = stallStandard simConfig stalls
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
