{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Tests.Protocols.PacketStream.PacketFifo (
  tests,
) where

import Clash.Prelude

import Data.Int (Int16)
import qualified Data.List as L

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Prelude as P

import Test.Tasty
import Test.Tasty.Hedgehog (HedgehogTestLimit (HedgehogTestLimit))
import Test.Tasty.Hedgehog.Extra (testProperty)
import Test.Tasty.TH (testGroupGenerator)

import Protocols
import Protocols.Hedgehog
import Protocols.PacketStream.Base
import Protocols.PacketStream.Hedgehog
import Protocols.PacketStream.PacketFifo

-- | Drops packets that consist of more than 2^n transfers.
dropBigPackets ::
  SNat n ->
  [PacketStreamM2S dataWidth meta] ->
  [PacketStreamM2S dataWidth meta]
dropBigPackets n packets =
  L.concat
    $ L.filter
      (\p -> L.length p < 2 P.^ snatToInteger n)
      (chunkByPacket packets)

-- | Test for id and proper dropping of aborted packets.
prop_packet_fifo_id :: Property
prop_packet_fifo_id =
  idWithModelSingleDomain
    @System
    defExpectOptions
    (genPackets 1 10 (genValidPacket defPacketOptions Gen.enumBounded (Range.linear 0 10)))
    (exposeClockResetEnable dropAbortedPackets)
    (exposeClockResetEnable (packetFifoC @_ @1 @Int16 d10 d10 Backpressure))

{- |
Ensure that backpressure because of a full content RAM and dropping of packets
that are too big to fit in the FIFO is tested.
-}
prop_packet_fifo_small_buffer_id :: Property
prop_packet_fifo_small_buffer_id =
  idWithModelSingleDomain
    @System
    defExpectOptions{eoStopAfterEmpty = 1000}
    (genPackets 1 10 (genValidPacket defPacketOptions Gen.enumBounded (Range.linear 0 30)))
    (exposeClockResetEnable (dropBigPackets d3 . dropAbortedPackets))
    (exposeClockResetEnable (packetFifoC @_ @1 @Int16 d3 d5 Backpressure))

{- |
Test for id using a small meta buffer to ensure backpressure using
the meta buffer is tested.
-}
prop_packet_fifo_small_meta_buffer_id :: Property
prop_packet_fifo_small_meta_buffer_id =
  idWithModelSingleDomain
    @System
    defExpectOptions
    (genPackets 1 30 (genValidPacket defPacketOptions Gen.enumBounded (Range.linear 0 4)))
    (exposeClockResetEnable dropAbortedPackets)
    (exposeClockResetEnable (packetFifoC @_ @1 @Int16 d10 d2 Backpressure))

-- | test for id and proper dropping of aborted packets
prop_overFlowDrop_packetFifo_id :: Property
prop_overFlowDrop_packetFifo_id =
  idWithModelSingleDomain
    @System
    defExpectOptions{eoStopAfterEmpty = 1000}
    (genPackets 1 10 (genValidPacket defPacketOptions Gen.enumBounded (Range.linear 0 10)))
    (exposeClockResetEnable dropAbortedPackets)
    (exposeClockResetEnable (packetFifoC @_ @1 @Int16 d10 d10 Drop))

-- | test for proper dropping when full
prop_overFlowDrop_packetFifo_drop :: Property
prop_overFlowDrop_packetFifo_drop =
  propWithModelSingleDomain
    @System
    defExpectOptions
    -- make sure the timeout is long as the packetFifo can be quiet for a while while dropping
    (liftA3 (\a b c -> a L.++ b L.++ c) genSmall genBig genSmall)
    (exposeClockResetEnable id)
    (exposeClockResetEnable (packetFifoC @_ @4 @Int16 d3 d5 Drop))
    (\xs ys -> diff ys L.isSubsequenceOf xs)
 where
  genSmall =
    genValidPacket defPacketOptions{poAbortMode = NoAbort} Gen.enumBounded (Range.linear 0 3)
  genBig =
    genValidPacket
      defPacketOptions{poAbortMode = NoAbort}
      Gen.enumBounded
      (Range.linear 9 9)

-- | test to check if there are no gaps inside of packets
prop_packetFifo_no_gaps :: Property
prop_packetFifo_no_gaps = property $ do
  let maxInputSize = 50
      ckt =
        exposeClockResetEnable
          (packetFifoC d12 d12 Backpressure)
          systemClockGen
          resetGen
          enableGen
      gen =
        genPackets
          1
          10
          ( genValidPacket defPacketOptions{poAbortMode = NoAbort} Gen.enumBounded (Range.linear 0 10)
          )

  packets :: [PacketStreamM2S 4 Int16] <- forAll gen

  let packetSize = 2 P.^ snatToInteger d12
      cfg = SimulationConfig 1 (2 * packetSize) False
      cktResult = simulateC ckt cfg (Just <$> packets)

  assert $ noGaps $ L.take (5 * maxInputSize) cktResult
 where
  noGaps :: [Maybe (PacketStreamM2S 4 Int16)] -> Bool
  noGaps (Just (PacketStreamM2S{_last = Nothing}) : Nothing : _) = False
  noGaps (_ : xs) = noGaps xs
  noGaps _ = True

tests :: TestTree
tests =
  localOption (mkTimeout 20_000_000 {- 20 seconds -})
    $ localOption
      (HedgehogTestLimit (Just 500))
      $(testGroupGenerator)
