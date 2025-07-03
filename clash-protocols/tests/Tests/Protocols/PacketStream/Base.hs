{-# LANGUAGE NoImplicitPrelude #-}

module Tests.Protocols.PacketStream.Base (
  tests,
) where

import Clash.Prelude

import Data.List qualified as L
import "extra" Data.List.Extra (unsnoc)

import Hedgehog (Property)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Test.Tasty (TestTree, localOption, mkTimeout)
import Test.Tasty.Hedgehog (HedgehogTestLimit (HedgehogTestLimit))
import Test.Tasty.Hedgehog.Extra (testProperty)
import Test.Tasty.TH (testGroupGenerator)

import Protocols.Hedgehog
import Protocols.PacketStream.Base
import Protocols.PacketStream.Hedgehog
import qualified Hedgehog as H

prop_strip_trailing_empty :: Property
prop_strip_trailing_empty = H.property $
  idWithModelSingleDomain
    @System
    defExpectOptions
    (genPackets 1 10 (genValidPacket defPacketOptions Gen.enumBounded (Range.linear 0 10)))
    (exposeClockResetEnable model')
    (exposeClockResetEnable (stripTrailingEmptyC @1 @Char))
 where
  model' packets = L.concatMap model (chunkByPacket packets)

  model :: [PacketStreamM2S 1 Char] -> [PacketStreamM2S 1 Char]
  model packet = case unsnoc packet of
    Nothing -> []
    Just (xs, l) -> case unsnoc xs of
      -- Preserve packets that consist of a single zero-byte transfer.
      Nothing -> [l]
      Just (ys, l2) ->
        if _last l == Just 0
          then ys L.++ [l2{_last = Just maxBound, _abort = _abort l2 || _abort l}]
          else packet

tests :: TestTree
tests =
  localOption (mkTimeout 20_000_000 {- 20 seconds -})
    $ localOption
      (HedgehogTestLimit (Just 500))
      $(testGroupGenerator)
