{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Tests.Protocols.PacketStream.Routing (
  tests,
) where

import Clash.Prelude

import Hedgehog hiding (Parallel)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Test.Tasty
import Test.Tasty.Hedgehog (HedgehogTestLimit (HedgehogTestLimit))
import Test.Tasty.Hedgehog.Extra (testProperty)
import Test.Tasty.TH (testGroupGenerator)

import qualified Protocols.Df as Df
import Protocols.Hedgehog
import Protocols.PacketStream.Base
import Protocols.PacketStream.Hedgehog
import Protocols.PacketStream.Routing

import qualified Data.List as L

{- |
Tests a packet arbiter for any data width and number of sources. In particular,
tests that packets from all sources are sent out unmodified in the same order
they were in in the source streams.
-}
makePropPacketArbiter ::
  forall sources dataWidth.
  (1 <= sources) =>
  (1 <= dataWidth) =>
  SNat sources ->
  SNat dataWidth ->
  Df.CollectMode ->
  Property
makePropPacketArbiter SNat SNat mode =
  propWithModelSingleDomain
    @System
    defExpectOptions{eoSampleMax = 1000}
    genSources
    (exposeClockResetEnable L.concat)
    (exposeClockResetEnable (packetArbiterC mode))
    (\xs ys -> partitionPackets xs === partitionPackets ys)
 where
  (minPackets, maxPackets) = case mode of
    -- NoSkip mode needs the same amount of packets generated for each
    -- source. Otherwise, starvation happens and the test won't end.
    Df.NoSkip -> (5, 5)
    _ -> (1, 10)
  genSources = mapM setMeta (indicesI @sources)
  setMeta j = do
    pkts <-
      genPackets
        @dataWidth
        minPackets
        maxPackets
        (genValidPacket defPacketOptions (pure ()) (Range.linear 0 10))
    pure $ L.map (\pkt -> pkt{_meta = j}) pkts

  partitionPackets packets =
    L.sortOn getMeta
      $ L.groupBy (\a b -> _meta a == _meta b)
      <$> chunkByPacket packets

  getMeta ((pkt : _) : _) = _meta pkt
  getMeta _ = error "makePropPacketArbiter: empty partition"

{- |
Generic test function for the packet dispatcher, testing for all data widths,
dispatch functions, and some meta types.
-}
makePropPacketDispatcher ::
  forall sinks dataWidth meta.
  (KnownNat sinks) =>
  (1 <= sinks) =>
  (1 <= dataWidth) =>
  (TestType meta) =>
  (Bounded meta) =>
  (Enum meta) =>
  (BitPack meta) =>
  SNat dataWidth ->
  -- | Dispatch function
  Vec sinks (meta -> Bool) ->
  Property
makePropPacketDispatcher SNat fs =
  idWithModelSingleDomain @System
    defExpectOptions{eoSampleMax = 2000, eoStopAfterEmpty = 1000}
    (genPackets 1 10 (genValidPacket defPacketOptions Gen.enumBounded (Range.linear 0 6)))
    (exposeClockResetEnable (model 0))
    (exposeClockResetEnable (packetDispatcherC fs))
 where
  model ::
    Index sinks ->
    [PacketStreamM2S dataWidth meta] ->
    Vec sinks [PacketStreamM2S dataWidth meta]
  model _ [] = pure []
  model i (y : ys)
    | (fs !! i) (_meta y) =
        let next = model 0 ys
         in replace i (y : (next !! i)) next
    | i < maxBound = model (i + 1) (y : ys)
    | otherwise = model 0 ys

-- | Tests the @NoSkip@ packet arbiter with one source; essentially an id test.
prop_packet_arbiter_noskip_id :: Property
prop_packet_arbiter_noskip_id = makePropPacketArbiter d1 d2 Df.NoSkip

-- | Tests the @Skip@ packet arbiter with one source; essentially an id test.
prop_packet_arbiter_skip_id :: Property
prop_packet_arbiter_skip_id = makePropPacketArbiter d1 d2 Df.Skip

-- | Tests the @Parallel@ packet arbiter with one source; essentially an id test.
prop_packet_arbiter_parallel_id :: Property
prop_packet_arbiter_parallel_id = makePropPacketArbiter d1 d2 Df.Parallel

-- | Tests the @NoSkip@ arbiter with five sources.
prop_packet_arbiter_noskip :: Property
prop_packet_arbiter_noskip = makePropPacketArbiter d5 d2 Df.NoSkip

-- | Tests the @Skip@ arbiter with five sources.
prop_packet_arbiter_skip :: Property
prop_packet_arbiter_skip = makePropPacketArbiter d5 d2 Df.Skip

-- | Tests the @Parallel@ arbiter with five sources.
prop_packet_arbiter_parallel :: Property
prop_packet_arbiter_parallel = makePropPacketArbiter d5 d2 Df.Parallel

{- |
Tests that the packet dispatcher works correctly with one sink that accepts
all packets; essentially an id test.
-}
prop_packet_dispatcher_id :: Property
prop_packet_dispatcher_id =
  makePropPacketDispatcher
    d4
    ((const True :: Int -> Bool) :> Nil)

{- |
Tests the packet dispatcher for a data width of four bytes and three
overlapping but incomplete dispatch functions, effectively testing whether
the circuit sends input to the first allowed output channel and drops input
if there are none.
-}
prop_packet_dispatcher :: Property
prop_packet_dispatcher = makePropPacketDispatcher d4 fs
 where
  fs :: Vec 3 (Index 4 -> Bool)
  fs =
    (>= 3)
      :> (>= 2)
      :> (>= 1)
      :> Nil

tests :: TestTree
tests =
  localOption (mkTimeout 20_000_000 {- 20 seconds -})
    $ localOption
      (HedgehogTestLimit (Just 500))
      $(testGroupGenerator)
