{-|
Defines ReadData channel of full AXI4 protocol with port names corresponding
to the AXI4 specification.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-missing-fields #-}

module Protocols.Axi4.Partial.Full.ReadData
  ( M2S_ReadData(..)
  , S2M_ReadData(..)
  , Axi4ReadData
  ) where

-- base
import Data.Coerce (coerce)
import Data.Kind (Type)
import GHC.Generics (Generic)
import Data.Proxy

-- clash-prelude
import qualified Clash.Prelude as C
import Clash.Prelude ((:::))

-- me
import Protocols.Axi4.Common
import Protocols.Internal
import Protocols.DfLike (DfLike)
import qualified Protocols.DfLike as DfLike

-- | AXI4 Read Data channel protocol
data Axi4ReadData
  (dom :: C.Domain)
  (kr :: KeepResponse)
  (iw :: IdWidth)
  (dataType :: Type)
  (userType :: Type)

instance Protocol (Axi4ReadData dom kr iw dataType userType) where
  type Fwd (Axi4ReadData dom kr iw dataType userType) =
    C.Signal dom (S2M_ReadData kr iw dataType userType)
  type Bwd (Axi4ReadData dom kr iw dataType userType) =
    C.Signal dom M2S_ReadData

instance Backpressure (Axi4ReadData dom kr iw dataType userType) where
  boolsToBwd _ = C.fromList_lazy . coerce

instance DfLike dom (Axi4ReadData dom kr iw dataType) userType where
  type Data (Axi4ReadData dom kr iw dataType) userType =
    S2M_ReadData kr iw dataType userType

  type Payload userType = userType

  type Ack (Axi4ReadData dom kr iw dataType) userType =
    M2S_ReadData

  getPayload _ (S2M_ReadData{_rvalid=True, _ruser}) = Just _ruser
  getPayload _ _ = Nothing
  {-# INLINE getPayload #-}

  setPayload _ _ dat (Just b) = dat{_rvalid=True, _ruser=b}
  setPayload _ dfB _ Nothing = DfLike.noData dfB
  {-# INLINE setPayload #-}

  noData _ = S2M_ReadData{_rvalid=False}
  {-# INLINE noData #-}

  boolToAck _ = coerce
  {-# INLINE boolToAck #-}

  ackToBool _ = coerce
  {-# INLINE ackToBool #-}

instance (C.KnownDomain dom, C.NFDataX userType, C.ShowX userType, Show userType) =>
  Simulate (Axi4ReadData dom kr iw dataType userType) where

  type SimulateType (Axi4ReadData dom kr iw dataType userType) =
    [S2M_ReadData kr iw dataType userType]

  type ExpectType (Axi4ReadData dom kr iw dataType userType) =
    [S2M_ReadData kr iw dataType userType]

  type SimulateChannels (Axi4ReadData dom kr iw dataType userType) = 1

  toSimulateType _ = id
  fromSimulateType _ = id

  driveC = DfLike.drive Proxy
  sampleC = DfLike.sample Proxy
  stallC conf (C.head -> (stallAck, stalls)) =
    DfLike.stall Proxy conf stallAck stalls

-- | See Table A2-6 "Read data channel signals"
data S2M_ReadData
  (kr :: KeepResponse)
  (iw :: IdWidth)
  (dataType :: Type)
  (userType :: Type) =
  S2M_ReadData
    { -- | Read address id*
      _rid :: "RID"    ::: C.BitVector (Width iw)

    , -- | Read data
      _rdata :: "RDATA" ::: dataType

      -- | Read response
    , _rresp :: "RRESP" ::: ResponseType kr

      -- | Read last
    , _rlast :: "RLAST" ::: Bool

      -- | Read valid
    , _rvalid :: "RVALID" ::: Bool

      -- | User data
    , _ruser :: "RUSER" ::: userType
    }
  deriving (Generic)

-- | See Table A2-6 "Read data channel signals"
newtype M2S_ReadData = M2S_ReadData
  { _rready :: "RREADY" ::: Bool }
  deriving (Show, Generic, C.NFDataX)

deriving instance
  ( C.NFDataX userType
  , C.NFDataX dataType
  , C.NFDataX (ResponseType kr) ) =>
  C.NFDataX (S2M_ReadData kr iw dataType userType)

deriving instance
  ( C.KnownNat (Width iw)
  , Show userType
  , Show dataType
  , Show (ResponseType kr) ) =>
  Show (S2M_ReadData kr iw dataType userType)
