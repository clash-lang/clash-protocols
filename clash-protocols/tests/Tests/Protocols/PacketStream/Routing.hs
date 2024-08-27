{-# LANGUAGE NumericUnderscores #-}

module Tests.Protocols.PacketStream.Routing (
  tests,
) where

import Clash.Prelude
import qualified Clash.Prelude as C

import Hedgehog hiding (Parallel)
import qualified Hedgehog.Range as Range

import Test.Tasty
import Test.Tasty.Hedgehog (HedgehogTestLimit (HedgehogTestLimit))
import Test.Tasty.Hedgehog.Extra (testProperty)
import Test.Tasty.TH (testGroupGenerator)

import Protocols.Hedgehog
import Protocols.PacketStream.Base
import Protocols.PacketStream.Hedgehog
import Protocols.PacketStream.Routing

import qualified Data.List as L

-- | Tests the round-robin packet arbiter with one source; essentially an id test
prop_packetarbiter_roundrobin_id :: Property
prop_packetarbiter_roundrobin_id = makePropPacketArbiter d1 d2 RoundRobin

-- | Tests the parallel packet arbiter with one source; essentially an id test
prop_packetarbiter_parallel_id :: Property
prop_packetarbiter_parallel_id = makePropPacketArbiter d1 d2 Parallel

-- Tests the round-robin arbiter with five sources
prop_packetarbiter_roundrobin :: Property
prop_packetarbiter_roundrobin = makePropPacketArbiter d5 d2 RoundRobin

-- Tests the parallel arbiter with five sources
prop_packetarbiter_parallel :: Property
prop_packetarbiter_parallel = makePropPacketArbiter d5 d2 Parallel

{- | Tests a packet arbiter for any data width and number of sources. In particular,
tests that packets from all sources are sent out unmodified in the same order
they were in in the source streams.
-}
makePropPacketArbiter ::
  forall p n.
  ( KnownNat p
  , 1 <= p
  , KnownNat n
  , 1 <= n
  ) =>
  SNat p ->
  SNat n ->
  ArbiterMode ->
  Property
makePropPacketArbiter _ _ mode =
  propWithModelSingleDomain
    @System
    defExpectOptions{eoSampleMax = 1000}
    genSources
    (exposeClockResetEnable L.concat)
    (exposeClockResetEnable (packetArbiterC mode))
    (\xs ys -> partitionPackets xs === partitionPackets ys)
 where
  genSources = mapM setMeta (indicesI @p)
  setMeta j = do
    pkts <- genValidPackets @n @() (Range.linear 1 10) (Range.linear 1 10) Abort
    pure $ L.map (\pkt -> pkt{_meta = j}) pkts

  partitionPackets packets =
    L.sortOn (_meta . L.head . L.head) $
      L.groupBy (\a b -> _meta a == _meta b) <$> chunkByPacket packets

{- | Tests that the packet dispatcher works correctly with one sink that accepts
all packets; essentially an id test.
-}
prop_packetdispatcher_id :: Property
prop_packetdispatcher_id =
  makePropPacketDispatcher
    d4
    ((const True :: Int -> Bool) :> Nil)

{- | Tests the packet dispatcher for a data width of four bytes and three
overlapping but incomplete dispatch functions, effectively testing whether
the circuit sends input to the first allowed output channel and drops input
if there are none.
-}
prop_packetdispatcher :: Property
prop_packetdispatcher = makePropPacketDispatcher d4 fs
 where
  fs :: Vec 3 (Index 4 -> Bool)
  fs =
    (>= 3)
      :> (>= 2)
      :> (>= 1)
      :> Nil

{- | Generic test function for the packet dispatcher, testing for all data widths,
dispatch functions, and some meta types
-}
makePropPacketDispatcher ::
  forall (p :: Nat) (dataWidth :: Nat) (a :: Type).
  ( KnownNat p
  , 1 <= p
  , KnownNat dataWidth
  , 1 <= dataWidth
  , TestType a
  , Bounded a
  , BitPack a
  ) =>
  SNat dataWidth ->
  Vec p (a -> Bool) ->
  Property
makePropPacketDispatcher _ fs =
  idWithModelSingleDomain @System
    defExpectOptions{eoSampleMax = 2000, eoStopAfterEmpty = 1000}
    (genValidPackets (Range.linear 1 10) (Range.linear 1 6) Abort)
    (exposeClockResetEnable (model 0))
    (exposeClockResetEnable (packetDispatcherC fs))
 where
  model ::
    Index p -> [PacketStreamM2S dataWidth a] -> Vec p [PacketStreamM2S dataWidth a]
  model _ [] = pure []
  model i (y : ys)
    | (fs C.!! i) (_meta y) = let next = model 0 ys in replace i (y : (next C.!! i)) next
    | i < maxBound = model (i + 1) (y : ys)
    | otherwise = model 0 ys

tests :: TestTree
tests =
  localOption (mkTimeout 20_000_000 {- 20 seconds -}) $
    localOption
      (HedgehogTestLimit (Just 100))
      $(testGroupGenerator)
