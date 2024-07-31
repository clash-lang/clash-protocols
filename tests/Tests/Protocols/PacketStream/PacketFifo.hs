{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}

module Tests.Protocols.PacketStream.PacketFifo where

-- base
import Data.Int (Int16)
import Prelude

-- clash-prelude
import Clash.Prelude hiding (drop, take, undefined, (++))
import qualified Clash.Prelude as C

-- hedgehog
import Hedgehog as H
import qualified Hedgehog.Range as Range

-- tasty
import Test.Tasty
import Test.Tasty.Hedgehog (HedgehogTestLimit (HedgehogTestLimit))
import Test.Tasty.Hedgehog.Extra (testProperty)
import Test.Tasty.TH (testGroupGenerator)

-- clash-protocols
import Protocols
import Protocols.Hedgehog
import Protocols.PacketStream.Base
import Protocols.PacketStream.PacketFifo (overflowDropPacketFifoC, packetFifoC)

-- tests
import Tests.Protocols.PacketStream.Base as U

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf (x : xs) (y : ys)
  | x == y = isSubsequenceOf xs ys
  | otherwise = isSubsequenceOf xs (y : ys)

-- | test for id and proper dropping of aborted packets
prop_packetFifo_id :: Property
prop_packetFifo_id =
  idWithModelSingleDomain
    @C.System
    defExpectOptions{eoSampleMax = 1000}
    (genValidPackets (Range.linear 1 30) (Range.linear 1 10) Abort)
    (C.exposeClockResetEnable dropAbortedPackets)
    (C.exposeClockResetEnable ckt)
 where
  ckt ::
    (HiddenClockResetEnable System) =>
    Circuit (PacketStream System 4 Int16) (PacketStream System 4 Int16)
  ckt = packetFifoC d12 d12

-- | test for id with a small buffer to ensure backpressure is tested
prop_packetFifo_small_buffer_id :: Property
prop_packetFifo_small_buffer_id =
  idWithModelSingleDomain
    @C.System
    defExpectOptions{eoSampleMax = 1000}
    (genValidPackets (Range.linear 1 10) (Range.linear 1 30) NoAbort)
    (C.exposeClockResetEnable dropAbortedPackets)
    (C.exposeClockResetEnable ckt)
 where
  ckt ::
    (HiddenClockResetEnable System) =>
    Circuit (PacketStream System 4 Int16) (PacketStream System 4 Int16)
  ckt = packetFifoC d5 d5

-- | test to check if there are no gaps inside of packets
prop_packetFifo_no_gaps :: Property
prop_packetFifo_no_gaps = property $ do
  let packetFifoSize = d12
      maxInputSize = 50
      ckt =
        exposeClockResetEnable
          (packetFifoC packetFifoSize packetFifoSize)
          systemClockGen
          resetGen
          enableGen
      gen = genValidPackets (Range.linear 1 10) (Range.linear 1 10) NoAbort

  packets :: [PacketStreamM2S 4 Int16] <- H.forAll gen

  let packetSize = 2 Prelude.^ snatToInteger packetFifoSize
      cfg = SimulationConfig 1 (2 * packetSize) False
      cktResult = simulateC ckt cfg (Just <$> packets)

  assert $ noGaps $ take (5 * maxInputSize) cktResult
 where
  noGaps :: [Maybe (PacketStreamM2S 4 Int16)] -> Bool
  noGaps (Just (PacketStreamM2S{_last = Nothing}) : Nothing : _) = False
  noGaps (_ : xs) = noGaps xs
  noGaps _ = True

-- | test for id and proper dropping of aborted packets
prop_overFlowDrop_packetFifo_id :: Property
prop_overFlowDrop_packetFifo_id =
  idWithModelSingleDomain
    @C.System
    defExpectOptions{eoSampleMax = 2000}
    (genValidPackets (Range.linear 1 30) (Range.linear 1 10) Abort)
    (C.exposeClockResetEnable dropAbortedPackets)
    (C.exposeClockResetEnable ckt)
 where
  ckt ::
    (HiddenClockResetEnable System) =>
    Circuit (PacketStream System 4 Int16) (PacketStream System 4 Int16)
  ckt = fromPacketStream |> overflowDropPacketFifoC d12 d12

-- | test for proper dropping when full
prop_overFlowDrop_packetFifo_drop :: Property
prop_overFlowDrop_packetFifo_drop =
  idWithModelSingleDomain
    @C.System
    defExpectOptions{eoSampleMax = 1000}
    -- make sure the timeout is long as the packetFifo can be quiet for a while while dropping
    (liftA3 (\a b c -> a ++ b ++ c) genSmall genBig genSmall)
    (C.exposeClockResetEnable model)
    (C.exposeClockResetEnable ckt)
 where
  ckt ::
    (HiddenClockResetEnable System) =>
    Circuit (PacketStream System 4 Int16) (PacketStream System 4 Int16)
  ckt = fromPacketStream |> overflowDropPacketFifoC d5 d5

  model :: [PacketStreamM2S 4 Int16] -> [PacketStreamM2S 4 Int16]
  model packets = Prelude.concat $ take 1 packetChunk ++ drop 2 packetChunk
   where
    packetChunk = chunkByPacket packets

  genSmall = genValidPacket (Range.linear 1 5) NoAbort
  genBig = genValidPacket (Range.linear 33 33) NoAbort

-- | test for id using a small metabuffer to ensure backpressure using the metabuffer is tested
prop_packetFifo_small_metaBuffer :: Property
prop_packetFifo_small_metaBuffer =
  idWithModelSingleDomain
    @C.System
    defExpectOptions
    (genValidPackets (Range.linear 1 30) (Range.linear 1 4) Abort)
    (C.exposeClockResetEnable dropAbortedPackets)
    (C.exposeClockResetEnable ckt)
 where
  ckt ::
    (HiddenClockResetEnable System) =>
    Circuit (PacketStream System 4 Int16) (PacketStream System 4 Int16)
  ckt = packetFifoC d12 d2

tests :: TestTree
tests =
  localOption (mkTimeout 20_000_000 {- 20 seconds -}) $
    localOption
      (HedgehogTestLimit (Just 100))
      $(testGroupGenerator)
