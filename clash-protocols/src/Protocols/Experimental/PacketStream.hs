{-# OPTIONS_GHC -fno-warn-orphans #-}

{- |
Experimental simulation and generic Hedgehog support for "Protocols.PacketStream".
-}
module Protocols.Experimental.PacketStream (
  module Protocols.PacketStream,
) where

import Clash.Prelude
import Control.DeepSeq (NFData)
import Data.Maybe qualified as Maybe
import Data.Proxy
import Prelude hiding (head)

import Protocols.Experimental.Df qualified as Df
import Protocols.Experimental.DfConv qualified as DfConv
import Protocols.Experimental.Hedgehog (Test (..))
import Protocols.Experimental.Simulate
import Protocols.PacketStream

instance Backpressure (PacketStream dom dataWidth meta) where
  boolsToBwd _ = fromList_lazy . fmap PacketStreamS2M

instance
  (KnownDomain dom) =>
  Simulate (PacketStream dom dataWidth meta)
  where
  type
    SimulateFwdType (PacketStream dom dataWidth meta) =
      [Maybe (PacketStreamM2S dataWidth meta)]
  type SimulateBwdType (PacketStream dom dataWidth meta) = [PacketStreamS2M]
  type SimulateChannels (PacketStream dom dataWidth meta) = 1

  simToSigFwd _ = fromList_lazy
  simToSigBwd _ = fromList_lazy
  sigToSimFwd _ s = sample_lazy s
  sigToSimBwd _ s = sample_lazy s

  stallC conf (head -> (stallAck, stalls)) =
    withClockResetEnable clockGen resetGen enableGen $
      DfConv.stall Proxy Proxy conf stallAck stalls

instance
  (KnownDomain dom) =>
  Drivable (PacketStream dom dataWidth meta)
  where
  type
    ExpectType (PacketStream dom dataWidth meta) =
      [PacketStreamM2S dataWidth meta]

  toSimulateType Proxy = fmap Just
  fromSimulateType Proxy = Maybe.catMaybes

  driveC conf vals =
    withClockResetEnable clockGen resetGen enableGen $
      DfConv.drive Proxy conf vals
  sampleC conf ckt =
    withClockResetEnable clockGen resetGen enableGen $
      DfConv.sample Proxy conf ckt

instance
  ( KnownNat dataWidth
  , NFDataX meta
  , NFData meta
  , ShowX meta
  , Show meta
  , Eq meta
  , KnownDomain dom
  ) =>
  Test (PacketStream dom dataWidth meta)
  where
  expectN Proxy options sampled =
    expectN (Proxy @(Df.Df dom _)) options sampled
