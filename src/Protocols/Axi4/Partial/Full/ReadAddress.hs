{-|
Defines ReadAddress channel of full AXI4 protocol with port names corresponding
to the AXI4 specification.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-missing-fields #-}

module Protocols.Axi4.Partial.Full.ReadAddress
  ( M2S_ReadAddress(..)
  , S2M_ReadAddress(..)
  , Axi4ReadAddress
  , toStrict, toStrictDf
  , fromStrict, fromStrictDf
  ) where

-- base
import Data.Coerce
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
import qualified Protocols.Axi4.Strict.Full.ReadAddress as Strict
import qualified Protocols.Df as Df
import Protocols.Df (Df)

-- | AXI4 Read Address channel protocol
data Axi4ReadAddress
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

instance Protocol (Axi4ReadAddress dom kb ksz lw iw aw kr kbl kl kc kp kq userType) where
  type Fwd (Axi4ReadAddress dom kb ksz lw iw aw kr kbl kl kc kp kq userType) =
    C.Signal dom (M2S_ReadAddress kb ksz lw iw aw kr kbl kl kc kp kq userType)
  type Bwd (Axi4ReadAddress dom kb ksz lw iw aw kr kbl kl kc kp kq userType) =
    C.Signal dom S2M_ReadAddress

instance Backpressure (Axi4ReadAddress dom kb ksz lw iw aw kr kbl kl kc kp kq userType) where
  boolsToBwd _ = C.fromList_lazy . coerce

instance DfLike dom (Axi4ReadAddress dom kb ksz lw iw aw kr kbl kl kc kp kq) userType where
  type Data (Axi4ReadAddress dom kb ksz lw iw aw kr kbl kl kc kp kq) userType =
    M2S_ReadAddress kb ksz lw iw aw kr kbl kl kc kp kq userType

  type Payload userType = userType

  type Ack (Axi4ReadAddress dom kb ksz lw iw aw kr kbl kl kc kp kq) userType =
    S2M_ReadAddress

  getPayload _ (M2S_ReadAddress{_arvalid=True, _aruser}) = Just _aruser
  getPayload _ _ = Nothing
  {-# INLINE getPayload #-}

  setPayload _ _ dat (Just b) = dat{_arvalid=True, _aruser=b}
  setPayload _ dfB _ Nothing = DfLike.noData dfB
  {-# INLINE setPayload #-}

  noData _ = M2S_ReadAddress{_arvalid=False}
  {-# INLINE noData #-}

  boolToAck _ = coerce
  {-# INLINE boolToAck #-}

  ackToBool _ = coerce
  {-# INLINE ackToBool #-}

instance (C.KnownDomain dom, C.NFDataX userType, C.ShowX userType, Show userType) =>
  Simulate (Axi4ReadAddress dom kb ksz lw iw aw kr kbl kl kc kp kq userType) where

  type SimulateType (Axi4ReadAddress dom kb ksz lw iw aw kr kbl kl kc kp kq userType) =
    [M2S_ReadAddress kb ksz lw iw aw kr kbl kl kc kp kq userType]

  type ExpectType (Axi4ReadAddress dom kb ksz lw iw aw kr kbl kl kc kp kq userType) =
    [M2S_ReadAddress kb ksz lw iw aw kr kbl kl kc kp kq userType]

  type SimulateChannels (Axi4ReadAddress dom kb ksz lw iw aw kr kbl kl kc kp kq userType) = 1

  toSimulateType _ = id
  fromSimulateType _ = id

  driveC = DfLike.drive Proxy
  sampleC = DfLike.sample Proxy
  stallC conf (C.head -> (stallAck, stalls)) =
    DfLike.stall Proxy conf stallAck stalls

-- | See Table A2-5 "Read address channel signals"
data M2S_ReadAddress
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
  M2S_ReadAddress
    { -- | Read address id*
      _arid :: "ARID"    ::: C.BitVector (Width iw)

      -- | Read address
    , _araddr :: "ARADDR" ::: C.BitVector (Width aw)

      -- | Read region*
    , _arregion:: "ARREGION" ::: RegionType kr

      -- | Burst length*
    , _arlen :: "ARLEN" ::: BurstLengthType kbl

      -- | Burst size*
    , _arsize :: "ARSIZE" ::: SizeType ksz

      -- | Burst type*
    , _arburst :: "ARBURST" ::: BurstType kb

      -- | Lock type*
    , _arlock :: "ARLOCK" ::: LockType kl

      -- | Cache type* (has been renamed to modifiable in AXI spec)
    , _arcache :: "ARCACHE" ::: CacheType kc

      -- | Protection type
    , _arprot :: "ARPROT" ::: PermissionsType kp

      -- | QoS value
    , _arqos :: "ARQOS" ::: QosType kq

      -- | Read address valid
    , _arvalid :: "ARVALID" ::: Bool

      -- | User data
    , _aruser :: "ARUSER" ::: userType
    }
  deriving (Generic)

-- | See Table A2-5 "Read address channel signals"
newtype S2M_ReadAddress = S2M_ReadAddress
  { _arready :: "ARREADY" ::: Bool }
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
  Show (M2S_ReadAddress kb ksz lw iw aw kr kbl kl kc kp kq userType)

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
  C.NFDataX (M2S_ReadAddress kb ksz lw iw aw kr kbl kl kc kp kq userType)

-- | Convert an 'Axi4ReadAddress' into its strict version
-- 'Strict.Axi4ReadAddress'. The latter is usually preferred in Clash designs
-- as its definitions don't contain partial fields. Note that the functions
-- defined over these circuits operate on @userType@. If you need functions to
-- map over all fields, consider using 'toStrictDf'.
toStrict ::
  Circuit
    (Axi4ReadAddress dom kb ksz lw iw aw kr kbl kl kc kp kq userType)
    (Strict.Axi4ReadAddress dom kb ksz lw iw aw kr kbl kl kc kp kq userType)
toStrict = Circuit (C.unbundle . fmap go . C.bundle)
 where
  go (M2S_ReadAddress{..}, ack)
    | _arvalid  = (coerce ack, Strict.M2S_ReadAddress{..})
    | otherwise = (coerce ack, Strict.M2S_NoReadAddress)

-- | Convert a 'Strict.Axi4ReadAddress' into 'Axi4ReadAddress'
fromStrict ::
  Circuit
    (Strict.Axi4ReadAddress dom kb ksz lw iw aw kr kbl kl kc kp kq userType)
    (Axi4ReadAddress dom kb ksz lw iw aw kr kbl kl kc kp kq userType)
fromStrict = Circuit (C.unbundle . fmap go . C.bundle)
 where
  go (Strict.M2S_ReadAddress{..}, ack) = (coerce ack, M2S_ReadAddress{_arvalid=True,..})
  go (Strict.M2S_NoReadAddress, ack) = (coerce ack, M2S_ReadAddress{_arvalid=False})

-- | Convert an 'Axi4ReadAddress' into its strict 'Df' equivalent.
toStrictDf ::
  Circuit
    (Axi4ReadAddress dom kb ksz lw iw aw kr kbl kl kc kp kq userType)
    (Df dom (Strict.M2S_ReadAddress kb ksz lw iw aw kr kbl kl kc kp kq userType))
toStrictDf = Circuit (C.unbundle . fmap go . C.bundle)
 where
  go (M2S_ReadAddress{..}, ack)
    | _arvalid  = (coerce ack, Df.Data (Strict.M2S_ReadAddress{..}))
    | otherwise = (coerce ack, Df.NoData)

-- | Convert a into 'Axi4ReadAddress' from its Df equivalent
fromStrictDf ::
  Circuit
    (Df dom (Strict.M2S_ReadAddress kb ksz lw iw aw kr kbl kl kc kp kq userType))
    (Axi4ReadAddress dom kb ksz lw iw aw kr kbl kl kc kp kq userType)
fromStrictDf = Circuit (C.unbundle . fmap go . C.bundle)
 where
  go (Df.Data (Strict.M2S_ReadAddress{..}), ack) = (coerce ack, M2S_ReadAddress{_arvalid=True,..})
  go (_, ack) = (coerce ack, M2S_ReadAddress{_arvalid=False})
