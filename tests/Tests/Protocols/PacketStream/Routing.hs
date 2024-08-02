{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}

module Tests.Protocols.PacketStream.Routing where

-- base
import Data.List (groupBy, sortOn)
import Prelude

-- clash-prelude
import Clash.Prelude (type (<=))
import qualified Clash.Prelude as C

-- hedgehog
import Hedgehog hiding (Parallel)
import qualified Hedgehog.Range as Range

-- tasty
import Test.Tasty
import Test.Tasty.Hedgehog (HedgehogTestLimit (HedgehogTestLimit))
import Test.Tasty.Hedgehog.Extra (testProperty)
import Test.Tasty.TH (testGroupGenerator)

-- clash-protocols

import Protocols.Hedgehog
import Protocols.PacketStream.Base
import Protocols.PacketStream.Routing

-- tests
import Tests.Protocols.PacketStream.Base

import qualified Data.List as L

-- | Tests the round-robin packet arbiter with one source; essentially an id test
prop_packetarbiter_roundrobin_id :: Property
prop_packetarbiter_roundrobin_id = makePropPacketArbiter C.d1 C.d2 RoundRobin

-- | Tests the parallel packet arbiter with one source; essentially an id test
prop_packetarbiter_parallel_id :: Property
prop_packetarbiter_parallel_id = makePropPacketArbiter C.d1 C.d2 Parallel

-- Tests the round-robin arbiter with five sources
prop_packetarbiter_roundrobin :: Property
prop_packetarbiter_roundrobin = makePropPacketArbiter C.d5 C.d2 RoundRobin

-- Tests the parallel arbiter with five sources
prop_packetarbiter_parallel :: Property
prop_packetarbiter_parallel = makePropPacketArbiter C.d5 C.d2 Parallel

{- | Tests a packet arbiter for any data width and number of sources. In particular,
tests that packets from all sources are sent out unmodified in the same order
they were in in the source streams.
-}
makePropPacketArbiter ::
  forall p n.
  ( C.KnownNat p
  , 1 <= p
  , C.KnownNat n
  , 1 <= n
  ) =>
  C.SNat p ->
  C.SNat n ->
  ArbiterMode ->
  Property
makePropPacketArbiter _ _ mode =
  propWithModelSingleDomain
    @C.System
    defExpectOptions{eoSampleMax = 1000}
    genSources
    (C.exposeClockResetEnable concat)
    (C.exposeClockResetEnable (packetArbiterC mode))
    (\xs ys -> partitionPackets xs === partitionPackets ys)
 where
  genSources = mapM setMeta (C.indicesI @p)
  setMeta j = do
    pkts <- genValidPackets @n @() (Range.linear 1 10) (Range.linear 1 10) Abort
    pure $ L.map (\pkt -> pkt{_meta = j}) pkts

  partitionPackets packets =
    sortOn (_meta . head . head) $
      groupBy (\a b -> _meta a == _meta b) <$> chunkByPacket packets

{- | Tests that the packet dispatcher works correctly with one sink that accepts
all packets; essentially an id test.
-}
prop_packetdispatcher_id :: Property
prop_packetdispatcher_id =
  makePropPacketDispatcher
    C.d4
    ((const True :: Int -> Bool) C.:> C.Nil)

{- | Tests the packet dispatcher for a data width of four bytes and three
overlapping but incomplete dispatch functions, effectively testing whether
the circuit sends input to the first allowed output channel and drops input
if there are none.
-}
prop_packetdispatcher :: Property
prop_packetdispatcher = makePropPacketDispatcher C.d4 fs
 where
  fs :: C.Vec 3 (C.Index 4 -> Bool)
  fs =
    (>= 3)
      C.:> (>= 2)
      C.:> (>= 1)
      C.:> C.Nil

{- | Generic test function for the packet dispatcher, testing for all data widths,
dispatch functions, and some meta types
-}
makePropPacketDispatcher ::
  forall (p :: C.Nat) (dataWidth :: C.Nat) (a :: C.Type).
  ( C.KnownNat p
  , 1 <= p
  , C.KnownNat dataWidth
  , 1 <= dataWidth
  , TestType a
  , Bounded a
  , C.BitPack a
  ) =>
  C.SNat dataWidth ->
  C.Vec p (a -> Bool) ->
  Property
makePropPacketDispatcher _ fs =
  idWithModelSingleDomain @C.System
    defExpectOptions{eoSampleMax = 2000, eoStopAfterEmpty = 1000}
    (genValidPackets (Range.linear 1 10) (Range.linear 1 6) Abort)
    (C.exposeClockResetEnable (model 0))
    (C.exposeClockResetEnable (packetDispatcherC fs))
 where
  model ::
    C.Index p -> [PacketStreamM2S dataWidth a] -> C.Vec p [PacketStreamM2S dataWidth a]
  model _ [] = pure []
  model i (y : ys)
    | (fs C.!! i) (_meta y) = let next = model 0 ys in C.replace i (y : (next C.!! i)) next
    | i < maxBound = model (i + 1) (y : ys)
    | otherwise = model 0 ys

tests :: TestTree
tests =
  localOption (mkTimeout 20_000_000 {- 20 seconds -}) $
    localOption
      (HedgehogTestLimit (Just 100))
      $(testGroupGenerator)
