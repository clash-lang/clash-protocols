{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}

module Tests.Protocols.PacketStream.PacketFifo (
  tests,
) where

import Clash.Prelude

import Data.Int (Int16)
import qualified Data.List as L

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Test.Tasty
import Test.Tasty.Hedgehog (HedgehogTestLimit (HedgehogTestLimit))
import Test.Tasty.Hedgehog.Extra (testProperty)
import Test.Tasty.TH (testGroupGenerator)

import Protocols
import Protocols.Hedgehog
import Protocols.PacketStream.Base
import Protocols.PacketStream.Hedgehog
import Protocols.PacketStream.PacketFifo

-- | test for id and proper dropping of aborted packets
prop_packetFifo_id :: Property
prop_packetFifo_id =
  idWithModelSingleDomain
    @System
    defExpectOptions{eoSampleMax = 1000, eoStopAfterEmpty = 1000}
    ( genPackets (Range.linear 1 30) Abort (genValidPacket Gen.enumBounded (Range.linear 1 10))
    )
    (exposeClockResetEnable dropAbortedPackets)
    (exposeClockResetEnable ckt)
 where
  ckt ::
    (HiddenClockResetEnable System) =>
    Circuit (PacketStream System 4 Int16) (PacketStream System 4 Int16)
  ckt = packetFifoC d12 d12 Backpressure

-- | test for id with a small buffer to ensure backpressure is tested
prop_packetFifo_small_buffer_id :: Property
prop_packetFifo_small_buffer_id =
  idWithModelSingleDomain
    @System
    defExpectOptions{eoSampleMax = 1000, eoStopAfterEmpty = 1000}
    ( genPackets (Range.linear 1 10) Abort (genValidPacket Gen.enumBounded (Range.linear 1 30))
    )
    (exposeClockResetEnable dropAbortedPackets)
    (exposeClockResetEnable ckt)
 where
  ckt ::
    (HiddenClockResetEnable System) =>
    Circuit (PacketStream System 4 Int16) (PacketStream System 4 Int16)
  ckt = packetFifoC d5 d5 Backpressure

-- | test to check if there are no gaps inside of packets
prop_packetFifo_no_gaps :: Property
prop_packetFifo_no_gaps = property $ do
  let packetFifoSize = d12
      maxInputSize = 50
      ckt =
        exposeClockResetEnable
          (packetFifoC packetFifoSize packetFifoSize Backpressure)
          systemClockGen
          resetGen
          enableGen
      gen =
        genPackets
          (Range.linear 1 10)
          NoAbort
          (genValidPacket Gen.enumBounded (Range.linear 1 10))

  packets :: [PacketStreamM2S 4 Int16] <- forAll gen

  let packetSize = 2 Prelude.^ snatToInteger packetFifoSize
      cfg = SimulationConfig 1 (2 * packetSize) False
      cktResult = simulateC ckt cfg (Just <$> packets)

  assert $ noGaps $ L.take (5 * maxInputSize) cktResult
 where
  noGaps :: [Maybe (PacketStreamM2S 4 Int16)] -> Bool
  noGaps (Just (PacketStreamM2S{_last = Nothing}) : Nothing : _) = False
  noGaps (_ : xs) = noGaps xs
  noGaps _ = True

-- | test for id and proper dropping of aborted packets
prop_overFlowDrop_packetFifo_id :: Property
prop_overFlowDrop_packetFifo_id =
  idWithModelSingleDomain
    @System
    defExpectOptions{eoSampleMax = 2000, eoStopAfterEmpty = 1000}
    ( genPackets (Range.linear 1 30) Abort (genValidPacket Gen.enumBounded (Range.linear 1 10))
    )
    (exposeClockResetEnable dropAbortedPackets)
    (exposeClockResetEnable ckt)
 where
  ckt ::
    (HiddenClockResetEnable System) =>
    Circuit (PacketStream System 4 Int16) (PacketStream System 4 Int16)
  ckt = packetFifoC d12 d12 Drop

-- | test for proper dropping when full
prop_overFlowDrop_packetFifo_drop :: Property
prop_overFlowDrop_packetFifo_drop =
  idWithModelSingleDomain
    @System
    defExpectOptions{eoSampleMax = 1000, eoStopAfterEmpty = 1000}
    -- make sure the timeout is long as the packetFifo can be quiet for a while while dropping
    (liftA3 (\a b c -> a L.++ b L.++ c) genSmall genBig genSmall)
    (exposeClockResetEnable model)
    (exposeClockResetEnable ckt)
 where
  ckt ::
    (HiddenClockResetEnable System) =>
    Circuit (PacketStream System 4 Int16) (PacketStream System 4 Int16)
  ckt = packetFifoC d5 d5 Drop

  model :: [PacketStreamM2S 4 Int16] -> [PacketStreamM2S 4 Int16]
  model packets = Prelude.concat $ L.take 1 packetChunk L.++ L.drop 2 packetChunk
   where
    packetChunk = chunkByPacket packets

  genSmall = genValidPacket Gen.enumBounded (Range.linear 1 5) NoAbort
  genBig = genValidPacket Gen.enumBounded (Range.linear 33 33) NoAbort

-- | test for id using a small metabuffer to ensure backpressure using the metabuffer is tested
prop_packetFifo_small_metaBuffer :: Property
prop_packetFifo_small_metaBuffer =
  idWithModelSingleDomain
    @System
    defExpectOptions
    (genPackets (Range.linear 1 30) Abort (genValidPacket Gen.enumBounded (Range.linear 1 4)))
    (exposeClockResetEnable dropAbortedPackets)
    (exposeClockResetEnable ckt)
 where
  ckt ::
    (HiddenClockResetEnable System) =>
    Circuit (PacketStream System 4 Int16) (PacketStream System 4 Int16)
  ckt = packetFifoC d12 d2 Backpressure

tests :: TestTree
tests =
  localOption (mkTimeout 20_000_000 {- 20 seconds -}) $
    localOption
      (HedgehogTestLimit (Just 100))
      $(testGroupGenerator)
