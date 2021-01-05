{-|
Defines WriteData channel of full AXI4 protocol with port names corresponding
to the AXI4 specification.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-missing-fields #-}

module Protocols.Axi4.Partial.Full.WriteData
  ( M2S_WriteData(..)
  , S2M_WriteData(..)
  , Axi4WriteData
  ) where

-- base
import Data.Coerce (coerce)
import Data.Kind (Type)
import GHC.Generics (Generic)
import GHC.TypeNats (Nat)
import Data.Proxy

-- clash-prelude
import qualified Clash.Prelude as C
import Clash.Prelude ((:::), type (*))

-- me
import Protocols.Axi4.Common
import Protocols.Internal
import Protocols.DfLike (DfLike)
import qualified Protocols.DfLike as DfLike

-- | AXI4 Write Data channel protocol
data Axi4WriteData
  (dom :: C.Domain)
  (ks :: KeepStrobe)
  (nBytes :: Nat)
  (userType :: Type)

instance Protocol (Axi4WriteData dom ks nBytes userType) where
  type Fwd (Axi4WriteData dom ks nBytes userType) =
    C.Signal dom (M2S_WriteData ks nBytes userType)
  type Bwd (Axi4WriteData dom ks nBytes userType) =
    C.Signal dom S2M_WriteData

instance Backpressure (Axi4WriteData dom ks dataType userType) where
  boolsToBwd _ = C.fromList_lazy . coerce

instance DfLike dom (Axi4WriteData dom ks dataType) userType where
  type Data (Axi4WriteData dom ks dataType) userType =
    M2S_WriteData ks dataType userType

  type Payload userType = userType

  type Ack (Axi4WriteData dom ks dataType) userType =
    S2M_WriteData

  getPayload _ (M2S_WriteData{_wvalid=True, _wuser}) = Just _wuser
  getPayload _ _ = Nothing
  {-# INLINE getPayload #-}

  setPayload _ _ dat (Just b) = dat{_wvalid=True, _wuser=b}
  setPayload _ dfB _ Nothing = DfLike.noData dfB
  {-# INLINE setPayload #-}

  noData _ = M2S_WriteData{_wvalid=False}
  {-# INLINE noData #-}

  boolToAck _ = coerce
  {-# INLINE boolToAck #-}

  ackToBool _ = coerce
  {-# INLINE ackToBool #-}

instance (C.KnownDomain dom, C.NFDataX userType, C.ShowX userType, Show userType) =>
  Simulate (Axi4WriteData dom ks nBytes userType) where

  type SimulateType (Axi4WriteData dom ks nBytes userType) =
    [M2S_WriteData ks nBytes userType]

  type ExpectType (Axi4WriteData dom ks nBytes userType) =
    [M2S_WriteData ks nBytes userType]

  type SimulateChannels (Axi4WriteData dom ks nBytes userType) = 1

  toSimulateType _ = id
  fromSimulateType _ = id

  driveC = DfLike.drive Proxy
  sampleC = DfLike.sample Proxy
  stallC conf (C.head -> (stallAck, stalls)) =
    DfLike.stall Proxy conf stallAck stalls

-- | See Table A2-3 "Write data channel signals"
data M2S_WriteData
  (ks :: KeepStrobe)
  (nBytes :: Nat)
  (userType :: Type) =
  M2S_WriteData
    { -- | Write data
      _wdata :: "WDATA" ::: C.BitVector (nBytes*8)

      -- | Write strobes
    , _wstrb :: "WSTRB" ::: StrobeType nBytes ks

      -- | Write last
    , _wlast :: "WLAST" ::: Bool

      -- | Write valid
    , _wvalid :: "WVALID" ::: Bool

      -- | User data
    , _wuser :: "WUSER" ::: userType
    }
  deriving (Generic)

-- | See Table A2-3 "Write data channel signals"
newtype S2M_WriteData = S2M_WriteData
  { _wready :: "WREADY" ::: Bool }
  deriving (Show, Generic, C.NFDataX)

deriving instance
  ( C.NFDataX userType
  , C.NFDataX (StrobeType nBytes ks) ) =>
  C.NFDataX (M2S_WriteData ks nBytes userType)

deriving instance
  ( Show userType
  , Show (StrobeType nBytes ks)
  , C.KnownNat nBytes ) =>
  Show (M2S_WriteData ks nBytes userType)
