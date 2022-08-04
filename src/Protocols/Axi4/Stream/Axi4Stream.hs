{-|
Types and instance declarations for the AXI4-stream protocol.
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- Hashable (Unsigned n)

module Protocols.Axi4.Stream.Axi4Stream where

-- base
import           Control.DeepSeq (NFData)
import           Data.Hashable (Hashable, hashWithSalt)
import qualified Data.Maybe as Maybe
import           Data.Proxy
import qualified Prelude as P

-- clash-prelude
import           Clash.Prelude hiding (take, concat, length)
import qualified Clash.Prelude as C

-- me
import           Protocols.Internal
import qualified Protocols.Df as Df
import qualified Protocols.DfConv as DfConv
import           Protocols.Hedgehog.Internal


instance (KnownNat n) => Hashable (Unsigned n)
instance (KnownNat n, Hashable a) => Hashable (Vec n a) where
  hashWithSalt s v = hashWithSalt s (toList v)

-- | Configuration for AXI4 Stream protocol. Determines the width of some
-- fields in 'Axi4StreamM2S'.
data Axi4StreamConfig
  = Axi4StreamConfig
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

-- | Shorthand for a "well-behaved" config, so that we don't need to write out
-- a bunch of type constraints later. Holds for every configuration; don't worry
-- about implementing this class.
type KnownAxi4StreamConfig conf =
  ( KnownNat (DataWidth conf)
  , KnownNat (IdWidth conf)
  , KnownNat (DestWidth conf)
  )

-- | Data sent from manager to subordinate. The tvalid field is left out: messages with @tvalid = False@
-- should be sent as a @NoAxi4StreamM2S@.
data Axi4StreamM2S (conf :: Axi4StreamConfig) (userType :: Type)
  = NoAxi4StreamM2S
  | Axi4StreamM2S
    { _tdata :: Vec (DataWidth conf) (Unsigned 8)
    , _tkeep :: Vec (DataWidth conf) Bool
    , _tstrb :: Vec (DataWidth conf) Bool
    , _tlast :: Bool
    , _tid   :: Unsigned (IdWidth conf)
    , _tdest :: Unsigned (DestWidth conf)
    , _tuser :: userType
    }
  deriving (Generic, C.ShowX, Show, NFData, Bundle)

deriving instance
  ( KnownAxi4StreamConfig conf
  , C.NFDataX userType
  ) => C.NFDataX (Axi4StreamM2S conf userType)

deriving instance
  ( KnownAxi4StreamConfig conf
  , Eq userType
  ) => Eq (Axi4StreamM2S conf userType)

-- | Info sent from manager to subordinate. Includes everything in
-- 'Axi4StreamM2S' constructor. Used in 'DfConv.DfConv' implementation.
data Axi4StreamInfo (conf :: Axi4StreamConfig) (userType :: Type)
  = Axi4StreamInfo
  { _tidata :: Vec (DataWidth conf) (Unsigned 8)
  , _tikeep :: Vec (DataWidth conf) Bool
  , _tistrb :: Vec (DataWidth conf) Bool
  , _tilast :: Bool
  , _tiid   :: Unsigned (IdWidth conf)
  , _tidest :: Unsigned (DestWidth conf)
  , _tiuser :: userType
  }
  deriving (Generic, C.ShowX, Show, NFData, Bundle)

deriving instance
  ( KnownAxi4StreamConfig conf
  , C.NFDataX userType
  ) => C.NFDataX (Axi4StreamInfo conf userType)

deriving instance
  ( KnownAxi4StreamConfig conf
  , Eq userType
  ) => Eq (Axi4StreamInfo conf userType)

deriving instance
  ( KnownAxi4StreamConfig conf
  , Hashable userType
  ) => Hashable (Axi4StreamInfo conf userType)

-- | Convert an 'Axi4StreamInfo' to an 'Axi4StreamM2S', keeping all the info.
axi4StreamInfoToM2S
  :: Maybe (Axi4StreamInfo conf userType)
  -> Axi4StreamM2S conf userType
axi4StreamInfoToM2S Nothing = NoAxi4StreamM2S
axi4StreamInfoToM2S (Just Axi4StreamInfo{..})
  = Axi4StreamM2S
  { _tdata = _tidata
  , _tkeep = _tikeep
  , _tstrb = _tistrb
  , _tlast = _tilast
  , _tid   = _tiid
  , _tdest = _tidest
  , _tuser = _tiuser
  }

-- | Convert an 'Axi4StreamM2S' to an 'Axi4StreamInfo', keeping all the info.
axi4StreamM2SToInfo
  :: Axi4StreamM2S conf userType
  -> Maybe (Axi4StreamInfo conf userType)
axi4StreamM2SToInfo NoAxi4StreamM2S = Nothing
axi4StreamM2SToInfo Axi4StreamM2S{..}
  = Just Axi4StreamInfo
  { _tidata = _tdata
  , _tikeep = _tkeep
  , _tistrb = _tstrb
  , _tilast = _tlast
  , _tiid   = _tid
  , _tidest = _tdest
  , _tiuser = _tuser
  }

-- | Data sent from subordinate to manager. A simple acknowledge message.
-- '_tready' may be on even when manager is sending 'NoAxi4StreamM2S'.
-- Manager may not decide whether or not to send 'NoAxi4StreamM2S' based on
-- the '_tready' signal.
newtype Axi4StreamS2M = Axi4StreamS2M { _tready :: Bool }
  deriving (Generic, C.NFDataX, C.ShowX, Eq, NFData, Show, Bundle)

-- | Type for AXI4 Stream protocol.
data Axi4Stream (dom :: Domain) (conf :: Axi4StreamConfig) (userType :: Type)

instance Protocol (Axi4Stream dom conf userType) where
  type Fwd (Axi4Stream dom conf userType) = Signal dom (Axi4StreamM2S conf userType)
  type Bwd (Axi4Stream dom conf userType) = Signal dom Axi4StreamS2M

instance Backpressure (Axi4Stream dom conf userType) where
  boolsToBwd _ = C.fromList_lazy . fmap Axi4StreamS2M

instance (KnownAxi4StreamConfig conf, NFDataX userType) =>
  DfConv.DfConv    (Axi4Stream dom conf userType) where
  type Dom         (Axi4Stream dom conf userType) = dom
  type FwdPayload  (Axi4Stream dom conf userType)
    = Axi4StreamInfo conf userType

  toDfCircuit proxy = DfConv.toDfCircuitHelper proxy s0 blankOtp stateFn where
    s0 = ()
    blankOtp = NoAxi4StreamM2S
    stateFn ack _ otpItem
      = pure (axi4StreamInfoToM2S otpItem, Nothing, Maybe.isJust otpItem && _tready ack)

  fromDfCircuit proxy = DfConv.fromDfCircuitHelper proxy s0 blankOtp stateFn where
    s0 = ()
    blankOtp = Axi4StreamS2M { _tready = False }
    stateFn m2s ack _
      = pure (Axi4StreamS2M { _tready = ack }, axi4StreamM2SToInfo m2s, False)

instance (KnownAxi4StreamConfig conf, NFDataX userType, KnownDomain dom) =>
  Simulate (Axi4Stream dom conf userType) where
  type SimulateFwdType (Axi4Stream dom conf userType) = [Axi4StreamM2S conf userType]
  type SimulateBwdType (Axi4Stream dom conf userType) = [Axi4StreamS2M]
  type SimulateChannels (Axi4Stream dom conf userType) = 1

  simToSigFwd _ = fromList_lazy
  simToSigBwd _ = fromList_lazy
  sigToSimFwd _ = sample_lazy
  sigToSimBwd _ = sample_lazy

  stallC conf (head -> (stallAck, stalls))
    = withClockResetEnable clockGen resetGen enableGen
    $ DfConv.stall Proxy Proxy conf stallAck stalls

instance (KnownAxi4StreamConfig conf, NFDataX userType, KnownDomain dom) =>
  Drivable (Axi4Stream dom conf userType) where
  type ExpectType (Axi4Stream dom conf userType)
    = [Axi4StreamInfo conf userType]

  toSimulateType Proxy = P.map (axi4StreamInfoToM2S . Just)
  fromSimulateType Proxy = Maybe.mapMaybe axi4StreamM2SToInfo

  driveC conf vals
    = withClockResetEnable clockGen resetGen enableGen
    $ DfConv.drive Proxy conf (axi4StreamM2SToInfo <$> vals)
  sampleC conf ckt
    = withClockResetEnable clockGen resetGen enableGen
    $ fmap axi4StreamInfoToM2S
    $ DfConv.sample Proxy conf
    $ ckt

instance
  ( KnownAxi4StreamConfig conf
  , NFDataX userType
  , NFData userType
  , ShowX userType
  , Show userType
  , Eq userType
  , KnownDomain dom ) =>
  Test (Axi4Stream dom conf userType) where

  expectToLengths Proxy = pure . P.length
  expectN Proxy options nExpected sampled
    = expectN (Proxy @(Df.Df dom _)) options nExpected
    $ Df.maybeToData . axi4StreamM2SToInfo <$> sampled
