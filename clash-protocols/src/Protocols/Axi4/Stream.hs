{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
-- Hashable (Unsigned n)
{-# OPTIONS_GHC -fno-warn-orphans #-}

{- |
Types and instance declarations for the AXI4-stream protocol.
-}
module Protocols.Axi4.Stream where

-- base
import Control.DeepSeq (NFData)
import Data.Hashable (Hashable, hashWithSalt)
import qualified Data.Maybe as Maybe
import Data.Proxy

-- clash-prelude
import Clash.Prelude hiding (concat, length, take)
import qualified Clash.Prelude as C

-- me

import qualified Protocols.Df as Df
import qualified Protocols.DfConv as DfConv
import Protocols.Hedgehog
import Protocols.Idle
import Protocols.Internal

instance (KnownNat n) => Hashable (Unsigned n)
instance (KnownNat n, Hashable a) => Hashable (Vec n a) where
  hashWithSalt s v = hashWithSalt s (toList v)

{- | Configuration for AXI4 Stream protocol. Determines the width of some
fields in 'Axi4StreamM2S'.
-}
data Axi4StreamConfig = Axi4StreamConfig
  { _dataWidth :: Nat
  , _idWidth :: Nat
  , _destWidth :: Nat
  }

-- | Grab '_dataWidth' at the type level.
type family DataWidth (conf :: Axi4StreamConfig) where
  DataWidth ('Axi4StreamConfig a _ _) = a

-- | Grab '_idWidth' at the type level.
type family IdWidth (conf :: Axi4StreamConfig) where
  IdWidth ('Axi4StreamConfig _ a _) = a

-- | Grab '_destWidth' at the type level.
type family DestWidth (conf :: Axi4StreamConfig) where
  DestWidth ('Axi4StreamConfig _ _ a) = a

{- | Shorthand for a "well-behaved" config, so that we don't need to write out
a bunch of type constraints later. Holds for every configuration; don't worry
about implementing this class.
-}
type KnownAxi4StreamConfig conf =
  ( KnownNat (DataWidth conf)
  , KnownNat (IdWidth conf)
  , KnownNat (DestWidth conf)
  )

{- | Data sent from manager to subordinate. The tvalid field is left out: messages with @tvalid = False@
should be sent as a @Nothing@.
-}
data Axi4StreamM2S (conf :: Axi4StreamConfig) (userType :: Type) = Axi4StreamM2S
  { _tdata :: Vec (DataWidth conf) (Unsigned 8)
  , _tkeep :: Vec (DataWidth conf) Bool
  , _tstrb :: Vec (DataWidth conf) Bool
  , _tlast :: Bool
  , _tid :: Unsigned (IdWidth conf)
  , _tdest :: Unsigned (DestWidth conf)
  , _tuser :: userType
  }
  deriving (Generic, C.ShowX, Show, NFData, Bundle)

deriving instance
  ( KnownAxi4StreamConfig conf
  , C.NFDataX userType
  ) =>
  C.NFDataX (Axi4StreamM2S conf userType)

deriving instance
  ( KnownAxi4StreamConfig conf
  , Eq userType
  ) =>
  Eq (Axi4StreamM2S conf userType)

deriving instance
  ( KnownAxi4StreamConfig conf
  , Hashable userType
  ) =>
  Hashable (Axi4StreamM2S conf userType)

{- | Data sent from subordinate to manager. A simple acknowledge message.
'_tready' may be on even when manager is sending 'Nothing'.
Manager may not decide whether or not to send 'Nothing' based on
the '_tready' signal.
-}
newtype Axi4StreamS2M = Axi4StreamS2M {_tready :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (C.NFDataX, C.ShowX, NFData, Bundle)

-- | Type for AXI4 Stream protocol.
data Axi4Stream (dom :: Domain) (conf :: Axi4StreamConfig) (userType :: Type)

instance Protocol (Axi4Stream dom conf userType) where
  type Fwd (Axi4Stream dom conf userType) = Signal dom (Maybe (Axi4StreamM2S conf userType))
  type Bwd (Axi4Stream dom conf userType) = Signal dom Axi4StreamS2M

instance Backpressure (Axi4Stream dom conf userType) where
  boolsToBwd _ = C.fromList_lazy . fmap Axi4StreamS2M

instance
  (KnownAxi4StreamConfig conf, NFDataX userType) =>
  DfConv.DfConv (Axi4Stream dom conf userType)
  where
  type Dom (Axi4Stream dom conf userType) = dom
  type
    FwdPayload (Axi4Stream dom conf userType) =
      Axi4StreamM2S conf userType

  toDfCircuit proxy = DfConv.toDfCircuitHelper proxy s0 blankOtp stateFn
   where
    s0 = ()
    blankOtp = Nothing
    stateFn ack _ otpItem =
      pure (otpItem, Nothing, Maybe.isJust otpItem C.&& _tready ack)

  fromDfCircuit proxy = DfConv.fromDfCircuitHelper proxy s0 blankOtp stateFn
   where
    s0 = ()
    blankOtp = Axi4StreamS2M{_tready = False}
    stateFn m2s ack _ =
      pure (Axi4StreamS2M{_tready = ack}, m2s, False)

instance
  (KnownAxi4StreamConfig conf, NFDataX userType, KnownDomain dom) =>
  Simulate (Axi4Stream dom conf userType)
  where
  type
    SimulateFwdType (Axi4Stream dom conf userType) =
      [Maybe (Axi4StreamM2S conf userType)]
  type SimulateBwdType (Axi4Stream dom conf userType) = [Axi4StreamS2M]
  type SimulateChannels (Axi4Stream dom conf userType) = 1

  simToSigFwd _ = fromList_lazy
  simToSigBwd _ = fromList_lazy
  sigToSimFwd _ s = sample_lazy s
  sigToSimBwd _ s = sample_lazy s

  stallC conf (C.head -> (stallAck, stalls)) =
    withClockResetEnable clockGen resetGen enableGen $
      DfConv.stall Proxy Proxy conf stallAck stalls

instance
  (KnownAxi4StreamConfig conf, NFDataX userType, KnownDomain dom) =>
  Drivable (Axi4Stream dom conf userType)
  where
  type
    ExpectType (Axi4Stream dom conf userType) =
      [Axi4StreamM2S conf userType]

  toSimulateType Proxy = fmap Just
  fromSimulateType Proxy = Maybe.catMaybes

  driveC conf vals =
    withClockResetEnable clockGen resetGen enableGen $
      DfConv.drive Proxy conf vals
  sampleC conf ckt =
    withClockResetEnable clockGen resetGen enableGen $
      DfConv.sample Proxy conf $
        ckt

instance
  ( KnownAxi4StreamConfig conf
  , NFDataX userType
  , NFData userType
  , ShowX userType
  , Show userType
  , Eq userType
  , KnownDomain dom
  ) =>
  Test (Axi4Stream dom conf userType)
  where
  expectN Proxy options sampled =
    expectN (Proxy @(Df.Df dom _)) options $
      Df.maybeToData
        <$> sampled

instance IdleCircuit (Axi4Stream dom conf userType) where
  idleFwd Proxy = C.pure Nothing
  idleBwd Proxy = C.pure $ Axi4StreamS2M False

{- | Force a /nack/ on the backward channel and /no data/ on the forward
channel if reset is asserted.
-}
forceResetSanity ::
  (KnownDomain dom, HiddenReset dom) =>
  Circuit (Axi4Stream dom conf userType) (Axi4Stream dom conf userType)
forceResetSanity = forceResetSanityGeneric
