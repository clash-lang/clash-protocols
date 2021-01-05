{-|
Defines WriteAddress channel of full AXI4 protocol with port names corresponding
to the AXI4 specification.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-missing-fields #-}

module Protocols.Axi4.Partial.Full.WriteAddress
  ( M2S_WriteAddress(..)
  , S2M_WriteAddress(..)
  , Axi4WriteAddress
  ) where

-- base
import Data.Coerce (coerce)
import Data.Kind (Type)
import Data.Proxy
import GHC.Generics (Generic)

-- clash-prelude
import qualified Clash.Prelude as C
import Clash.Prelude ((:::))

-- me
import Protocols.Axi4.Common
import Protocols.Internal
import Protocols.DfLike (DfLike)
import qualified Protocols.DfLike as DfLike

-- | AXI4 Write Address channel protocol
data Axi4WriteAddress
  (dom :: C.Domain)
  (kb :: KeepBurst)
  (ksz :: KeepSize)
  (lw :: LengthWidth)
  (iw :: IdWidth)
  (aw :: AddrWidth)
  (kr :: KeepRegion)
  (kbl :: KeepBurstLength)
  (kl :: KeepLock)
  (kc :: KeepCache)
  (kp :: KeepPermissions)
  (kq :: KeepQos)
  (userType :: Type)

instance Protocol (Axi4WriteAddress dom kb ksz lw iw aw kr kbl kl kc kp kq userType) where
  type Fwd (Axi4WriteAddress dom kb ksz lw iw aw kr kbl kl kc kp kq userType) =
    C.Signal dom (M2S_WriteAddress kb ksz lw iw aw kr kbl kl kc kp kq userType)
  type Bwd (Axi4WriteAddress dom kb ksz lw iw aw kr kbl kl kc kp kq userType) =
    C.Signal dom S2M_WriteAddress

instance Backpressure (Axi4WriteAddress dom kb ksz lw iw aw kr kbl kl kc kp kq userType) where
  boolsToBwd _ = C.fromList_lazy . coerce

instance DfLike dom (Axi4WriteAddress dom kb ksz lw iw aw kr kbl kl kc kp kq) userType where
  type Data (Axi4WriteAddress dom kb ksz lw iw aw kr kbl kl kc kp kq) userType =
    M2S_WriteAddress kb ksz lw iw aw kr kbl kl kc kp kq userType

  type Payload userType = userType

  type Ack (Axi4WriteAddress dom kb ksz lw iw aw kr kbl kl kc kp kq) userType =
    S2M_WriteAddress

  getPayload _ (M2S_WriteAddress{_awvalid=True, _awuser}) = Just _awuser
  getPayload _ _ = Nothing
  {-# INLINE getPayload #-}

  setPayload _ _ dat (Just b) = dat{_awvalid=True, _awuser=b}
  setPayload _ dfB _ Nothing = DfLike.noData dfB
  {-# INLINE setPayload #-}

  noData _ = M2S_WriteAddress{_awvalid=False}
  {-# INLINE noData #-}

  boolToAck _ = coerce
  {-# INLINE boolToAck #-}

  ackToBool _ = coerce
  {-# INLINE ackToBool #-}

instance (C.KnownDomain dom, C.NFDataX userType, C.ShowX userType, Show userType) =>
  Simulate (Axi4WriteAddress dom kb ksz lw iw aw kr kbl kl kc kp kq userType) where

  type SimulateType (Axi4WriteAddress dom kb ksz lw iw aw kr kbl kl kc kp kq userType) =
    [M2S_WriteAddress kb ksz lw iw aw kr kbl kl kc kp kq userType]

  type ExpectType (Axi4WriteAddress dom kb ksz lw iw aw kr kbl kl kc kp kq userType) =
    [M2S_WriteAddress kb ksz lw iw aw kr kbl kl kc kp kq userType]

  type SimulateChannels (Axi4WriteAddress dom kb ksz lw iw aw kr kbl kl kc kp kq userType) = 1

  toSimulateType _ = id
  fromSimulateType _ = id

  driveC = DfLike.drive Proxy
  sampleC = DfLike.sample Proxy
  stallC conf (C.head -> (stallAck, stalls)) =
    DfLike.stall Proxy conf stallAck stalls

-- | See Table A2-2 "Write address channel signals"
data M2S_WriteAddress
  (kb :: KeepBurst)
  (ksz :: KeepSize)
  (lw :: LengthWidth)
  (iw :: IdWidth)
  (aw :: AddrWidth)
  (kr :: KeepRegion)
  (kbl :: KeepBurstLength)
  (kl :: KeepLock)
  (kc :: KeepCache)
  (kp :: KeepPermissions)
  (kq :: KeepQos)
  (userType :: Type) =
  M2S_WriteAddress
    { -- | Write address id*
      _awid :: "AWID"    ::: C.BitVector (Width iw)

      -- | Write address
    , _awaddr :: "AWADDR" ::: C.BitVector (Width aw)

      -- | Write region*
    , _awregion:: "AWREGION" ::: RegionType kr

      -- | Burst length*
    , _awlen :: "AWLEN" ::: BurstLengthType kbl

      -- | Burst size*
    , _awsize :: "AWSIZE" ::: SizeType ksz

      -- | Burst type*
    , _awburst :: "AWBURST" ::: BurstType kb

      -- | Lock type*
    , _awlock :: "AWLOCK" ::: LockType kl

      -- | Cache type*
    , _awcache :: "AWCACHE" ::: CacheType kc

      -- | Protection type
    , _awprot :: "AWPROT" ::: PermissionsType kp

      -- | QoS value
    , _awqos :: "AWQOS" ::: QosType kq

      -- | Write address valid
    , _awvalid :: "AWVALID" ::: Bool

      -- | User data
    , _awuser :: "AWUSER" ::: userType
    }
  deriving (Generic)

-- | See Table A2-2 "Write address channel signals"
newtype S2M_WriteAddress = S2M_WriteAddress
  { _awready :: "AWREADY" ::: Bool }
  deriving (Show, Generic, C.NFDataX)

deriving instance
  ( C.KnownNat (Width iw)
  , C.KnownNat (Width aw)
  , Show (SizeType ksz)
  , Show (BurstType kb)
  , Show userType
  , Show (RegionType kr)
  , Show (BurstLengthType kbl)
  , Show (LockType kl)
  , Show (CacheType kc)
  , Show (PermissionsType kp)
  , Show (QosType kq) ) =>
  Show (M2S_WriteAddress kb ksz lw iw aw kr kbl kl kc kp kq userType)

deriving instance
  ( C.NFDataX userType
  , C.NFDataX (BurstType kb)
  , C.NFDataX (SizeType ksz)
  , C.NFDataX (BurstType kb)
  , C.NFDataX userType
  , C.NFDataX (RegionType kr)
  , C.NFDataX (BurstLengthType kbl)
  , C.NFDataX (LockType kl)
  , C.NFDataX (CacheType kc)
  , C.NFDataX (PermissionsType kp)
  , C.NFDataX (QosType kq) ) =>
  C.NFDataX (M2S_WriteAddress kb ksz lw iw aw kr kbl kl kc kp kq userType)
