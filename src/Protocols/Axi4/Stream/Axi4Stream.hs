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
type GoodAxi4StreamConfig conf =
  ( KnownNat (DataWidth conf)
  , KnownNat (IdWidth conf)
  , KnownNat (DestWidth conf)
  )

-- | A byte sent along an AXI4 Stream. Each byte can either be a data byte, a
-- position byte, or a null byte. The value of position and null bytes should
-- be ignored. Additionally, null bytes can be added or dropped.
data Axi4StreamByte
  = DataByte (Unsigned 8)
  | PositionByte
  | NullByte
  deriving (Generic, C.NFDataX, C.ShowX, Eq, NFData, Show, Hashable)

-- | Data sent from manager to subordinate. The tvalid field is left out: messages with @tvalid = False@
-- should be sent as a @NoAxi4StreamM2S@. The tdata, @tstrb@, and @tkeep@ fields are
-- all grouped in the @_tdata@ field in this datatype (see @Axi4StreamByte@).
data Axi4StreamM2S (conf :: Axi4StreamConfig) (userType :: Type)
  = NoAxi4StreamM2S
  | Axi4StreamM2S
    { _tdata :: Vec (DataWidth conf) Axi4StreamByte
    , _tlast :: Bool
    , _tid   :: Unsigned (IdWidth conf)
    , _tdest :: Unsigned (DestWidth conf)
    , _tuser :: userType
    }
  deriving (Generic, C.ShowX, Show, NFData, Bundle)

deriving instance
  ( GoodAxi4StreamConfig conf
  , C.NFDataX userType
  ) => C.NFDataX (Axi4StreamM2S conf userType)

deriving instance
  ( GoodAxi4StreamConfig conf
  , Eq userType
  ) => Eq (Axi4StreamM2S conf userType)

-- | "Unimportant" info sent from manager to subordinate. Includes everything in
-- 'Axi4StreamM2S' except for '_tdata'. Used in 'DfConv.DfConv' implementation.
data Axi4StreamExtraInfo (conf :: Axi4StreamConfig) (userType :: Type)
  = Axi4StreamExtraInfo
  { _telast :: Bool
  , _teid   :: Unsigned (IdWidth conf)
  , _tedest :: Unsigned (DestWidth conf)
  , _teuser :: userType
  }
  deriving (Generic, C.ShowX, Show, NFData, Bundle)

deriving instance
  ( GoodAxi4StreamConfig conf
  , C.NFDataX userType
  ) => C.NFDataX (Axi4StreamExtraInfo conf userType)

deriving instance
  ( GoodAxi4StreamConfig conf
  , Eq userType
  ) => Eq (Axi4StreamExtraInfo conf userType)

deriving instance
  ( GoodAxi4StreamConfig conf
  , Hashable userType
  ) => Hashable (Axi4StreamExtraInfo conf userType)

-- | Combine "important" data and "unimportant" extra info to make an 'Axi4StreamM2S'.
axi4StreamDataToM2S
  :: Maybe (Axi4StreamExtraInfo conf userType, Vec (DataWidth conf) Axi4StreamByte)
  -> Axi4StreamM2S conf userType
axi4StreamDataToM2S Nothing = NoAxi4StreamM2S
axi4StreamDataToM2S (Just (Axi4StreamExtraInfo{..}, _tdata))
  = Axi4StreamM2S
  { _tdata
  , _tlast = _telast
  , _tid   = _teid
  , _tdest = _tedest
  , _tuser = _teuser
  }

-- | Split an 'Axi4StreamM2S' into "important" data and "unimportant" extra info.
axi4StreamM2SToData
  :: Axi4StreamM2S conf userType
  -> Maybe (Axi4StreamExtraInfo conf userType, Vec (DataWidth conf) Axi4StreamByte)
axi4StreamM2SToData NoAxi4StreamM2S = Nothing
axi4StreamM2SToData Axi4StreamM2S{..}
  = Just (Axi4StreamExtraInfo
  { _telast = _tlast
  , _teid   = _tid
  , _tedest = _tdest
  , _teuser = _tuser
  }, _tdata)

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

instance (GoodAxi4StreamConfig conf, NFDataX userType) =>
  DfConv.DfConv    (Axi4Stream dom conf userType) where
  type Dom         (Axi4Stream dom conf userType) = dom
  type FwdPayload  (Axi4Stream dom conf userType)
    = (Axi4StreamExtraInfo conf userType, Vec (DataWidth conf) Axi4StreamByte)

  toDfCircuit proxy = DfConv.toDfCircuitHelper proxy s0 blankOtp stateFn where
    s0 = ()
    blankOtp = NoAxi4StreamM2S
    stateFn ack _ otpItem
      = pure (axi4StreamDataToM2S otpItem, Nothing, Maybe.isJust otpItem && _tready ack)

  fromDfCircuit proxy = DfConv.fromDfCircuitHelper proxy s0 blankOtp stateFn where
    s0 = ()
    blankOtp = Axi4StreamS2M { _tready = False }
    stateFn m2s ack _
      = pure (Axi4StreamS2M { _tready = ack }, axi4StreamM2SToData m2s, False)

instance (GoodAxi4StreamConfig conf, NFDataX userType, KnownDomain dom) =>
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

instance (GoodAxi4StreamConfig conf, NFDataX userType, KnownDomain dom) =>
  Drivable (Axi4Stream dom conf userType) where
  type ExpectType (Axi4Stream dom conf userType)
    = [(Axi4StreamExtraInfo conf userType, Vec (DataWidth conf) Axi4StreamByte)]

  toSimulateType Proxy = P.map (axi4StreamDataToM2S . Just)
  fromSimulateType Proxy = Maybe.mapMaybe axi4StreamM2SToData

  driveC conf vals
    = withClockResetEnable clockGen resetGen enableGen
    $ DfConv.drive Proxy conf (axi4StreamM2SToData <$> vals)
  sampleC conf ckt
    = withClockResetEnable clockGen resetGen enableGen
    $ fmap axi4StreamDataToM2S
    $ DfConv.sample Proxy conf
    $ ckt

instance
  ( GoodAxi4StreamConfig conf
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
    $ Df.maybeToData . axi4StreamM2SToData <$> sampled
