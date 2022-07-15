{-|
Defines ReadAddress channel of full AXI4 protocol with port names corresponding
to the AXI4 specification.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-missing-fields #-}

module Protocols.Axi4.ReadAddress
  ( M2S_ReadAddress(..)
  , S2M_ReadAddress(..)
  , Axi4ReadAddress
  , mapFull

    -- * configuration
  , Axi4ReadAddressConfig(..)
  , GoodAxi4ReadAddressConfig
  , ARKeepBurst
  , ARKeepSize
  , ARIdWidth
  , ARAddrWidth
  , ARKeepRegion
  , ARKeepBurstLength
  , ARKeepLock
  , ARKeepCache
  , ARKeepPermissions
  , ARKeepQos
  ) where

-- base
import Data.Coerce
import Data.Kind (Type)
import Data.Proxy
import GHC.Generics (Generic)

-- clash-prelude
import qualified Clash.Prelude as C

-- me
import Protocols.Axi4.Common
import Protocols.Internal
import Protocols.DfLike (DfLike)
import qualified Protocols.DfLike as DfLike

-- | Configuration options for 'Axi4ReadAddress'.
data Axi4ReadAddressConfig = Axi4ReadAddressConfig
  { _arKeepBurst       :: Bool
  , _arKeepSize        :: Bool
  , _arIdWidth         :: C.Nat
  , _arAddrWidth       :: C.Nat
  , _arKeepRegion      :: Bool
  , _arKeepBurstLength :: Bool
  , _arKeepLock        :: Bool
  , _arKeepCache       :: Bool
  , _arKeepPermissions :: Bool
  , _arKeepQos         :: Bool
  }

-- | Grab '_arKeepBurst' from 'Axi4ReadAddressConfig' at the type level.
-- This boolean value determines whether to keep the '_arburst' field
-- in 'M2S_ReadAddress'.
type family ARKeepBurst (conf :: Axi4ReadAddressConfig) where
  ARKeepBurst ('Axi4ReadAddressConfig a _ _ _ _ _ _ _ _ _) = a

-- | Grab '_arKeepSize' from 'Axi4ReadAddressConfig' at the type level.
-- This boolean value determines whether to keep the '_arsize' field
-- in 'M2S_ReadAddress'.
type family ARKeepSize (conf :: Axi4ReadAddressConfig) where
  ARKeepSize ('Axi4ReadAddressConfig _ a _ _ _ _ _ _ _ _) = a

-- | Grab '_arIdWidth' from 'Axi4ReadAddressConfig' at the type level.
-- This nat value determines the size of the '_arid' field
-- in 'M2S_ReadAddress'.
type family ARIdWidth (conf :: Axi4ReadAddressConfig) where
  ARIdWidth ('Axi4ReadAddressConfig _ _ a _ _ _ _ _ _ _) = a

-- | Grab '_arAddrWidth' from 'Axi4ReadAddressConfig' at the type level.
-- This nat value determines the size of the '_araddr' field
-- in 'M2S_ReadAddress'.
type family ARAddrWidth (conf :: Axi4ReadAddressConfig) where
  ARAddrWidth ('Axi4ReadAddressConfig _ _ _ a _ _ _ _ _ _) = a

-- | Grab '_arKeepRegion' from 'Axi4ReadAddressConfig' at the type level.
-- This boolean value determines whether to keep the '_arregion' field
-- in 'M2S_ReadAddress'.
type family ARKeepRegion (conf :: Axi4ReadAddressConfig) where
  ARKeepRegion ('Axi4ReadAddressConfig _ _ _ _ a _ _ _ _ _) = a

-- | Grab '_arKeepBurstLength' from 'Axi4ReadAddressConfig' at the type level.
-- This boolean value determines whether to keep the '_arlen' field
-- in 'M2S_ReadAddress'.
type family ARKeepBurstLength (conf :: Axi4ReadAddressConfig) where
  ARKeepBurstLength ('Axi4ReadAddressConfig _ _ _ _ _ a _ _ _ _) = a

-- | Grab '_arKeepLock' from 'Axi4ReadAddressConfig' at the type level.
-- This boolean value determines whether to keep the '_arlock' field
-- in 'M2S_ReadAddress'.
type family ARKeepLock (conf :: Axi4ReadAddressConfig) where
  ARKeepLock ('Axi4ReadAddressConfig _ _ _ _ _ _ a _ _ _) = a

-- | Grab '_arKeepCache' from 'Axi4ReadAddressConfig' at the type level.
-- This boolean value determines whether to keep the '_arcache' field
-- in 'M2S_ReadAddress'.
type family ARKeepCache (conf :: Axi4ReadAddressConfig) where
  ARKeepCache ('Axi4ReadAddressConfig _ _ _ _ _ _ _ a _ _) = a

-- | Grab '_arKeepPermissions' from 'Axi4ReadAddressConfig' at the type level.
-- This boolean value determines whether to keep the '_arprot' field
-- in 'M2S_ReadAddress'.
type family ARKeepPermissions (conf :: Axi4ReadAddressConfig) where
  ARKeepPermissions ('Axi4ReadAddressConfig _ _ _ _ _ _ _ _ a _) = a

-- | Grab '_arKeepQos' from 'Axi4ReadAddressConfig' at the type level.
-- This boolean value determines whether to keep the '_arqos' field
-- in 'M2S_ReadAddress'.
type family ARKeepQos (conf :: Axi4ReadAddressConfig) where
  ARKeepQos ('Axi4ReadAddressConfig _ _ _ _ _ _ _ _ _ a) = a

-- | AXI4 Read Address channel protocol
data Axi4ReadAddress
  (dom :: C.Domain)
  (conf :: Axi4ReadAddressConfig)
  (userType :: Type)

instance Protocol (Axi4ReadAddress dom conf userType) where
  type Fwd (Axi4ReadAddress dom conf userType) =
    C.Signal dom (M2S_ReadAddress conf userType)
  type Bwd (Axi4ReadAddress dom conf userType) =
    C.Signal dom S2M_ReadAddress

instance Backpressure (Axi4ReadAddress dom conf userType) where
  boolsToBwd _ = C.fromList_lazy . coerce

instance DfLike dom (Axi4ReadAddress dom conf) userType where
  type Data (Axi4ReadAddress dom conf) userType =
    M2S_ReadAddress conf userType

  type Payload userType = userType

  type Ack (Axi4ReadAddress dom conf) userType =
    S2M_ReadAddress

  getPayload _ (M2S_ReadAddress{_aruser}) = Just _aruser
  getPayload _ M2S_NoReadAddress = Nothing
  {-# INLINE getPayload #-}

  setPayload _ _ dat (Just b) = dat{_aruser=b}
  setPayload _ dfB _ Nothing = DfLike.noData dfB
  {-# INLINE setPayload #-}

  noData _ = M2S_NoReadAddress
  {-# INLINE noData #-}

  boolToAck _ = coerce
  {-# INLINE boolToAck #-}

  ackToBool _ = coerce
  {-# INLINE ackToBool #-}

instance (C.KnownDomain dom, C.NFDataX userType, C.ShowX userType, Show userType) =>
  Simulate (Axi4ReadAddress dom conf userType) where

  type SimulateFwdType (Axi4ReadAddress dom conf userType) =
    [M2S_ReadAddress conf userType]

  type SimulateBwdType (Axi4ReadAddress dom conf userType) =
    [S2M_ReadAddress]

  type SimulateChannels (Axi4ReadAddress dom conf userType) = 1

  simToSigFwd Proxy = C.fromList_lazy
  simToSigBwd Proxy = C.fromList_lazy
  sigToSimFwd Proxy = C.sample_lazy
  sigToSimBwd Proxy = C.sample_lazy

  stallC conf (C.head -> (stallAck, stalls)) =
    DfLike.stall Proxy conf stallAck stalls

-- | See Table A2-5 "Read address channel signals"

data M2S_ReadAddress
  (conf :: Axi4ReadAddressConfig)
  (userType :: Type)
  = M2S_NoReadAddress
  | M2S_ReadAddress
    { -- | Read address id*
      _arid :: !(C.BitVector (ARIdWidth conf))

      -- | Read address
    , _araddr :: !(C.BitVector (ARAddrWidth conf))

      -- | Read region*
    , _arregion :: !(RegionType (ARKeepRegion conf))

      -- | Burst length*
    , _arlen :: !(BurstLengthType (ARKeepBurstLength conf))

      -- | Burst size*
    , _arsize :: !(SizeType (ARKeepSize conf))

      -- | Burst type*
    , _arburst :: !(BurstType (ARKeepBurst conf))

      -- | Lock type*
    , _arlock :: !(LockType (ARKeepLock conf))

      -- | Cache type* (has been renamed to modifiable in AXI spec)
    , _arcache :: !(CacheType (ARKeepCache conf))

      -- | Protection type
    , _arprot :: !(PermissionsType (ARKeepPermissions conf))

      -- | QoS value
    , _arqos :: !(QosType (ARKeepQos conf))

      -- | User data
    , _aruser :: !userType
    }
  deriving (Generic)

-- | See Table A2-5 "Read address channel signals"
newtype S2M_ReadAddress = S2M_ReadAddress
  { _arready :: Bool }
  deriving (Show, Generic, C.NFDataX)

-- | Shorthand for a "well-behaved" read address config,
-- so that we don't need to write out a bunch of type constraints later.
-- Holds for every configuration; don't worry about implementing this class.
class
  ( KeepTypeClass (ARKeepBurst conf)
  , KeepTypeClass (ARKeepSize conf)
  , KeepTypeClass (ARKeepRegion conf)
  , KeepTypeClass (ARKeepBurstLength conf)
  , KeepTypeClass (ARKeepLock conf)
  , KeepTypeClass (ARKeepCache conf)
  , KeepTypeClass (ARKeepPermissions conf)
  , KeepTypeClass (ARKeepQos conf)

  , C.KnownNat (ARIdWidth conf)
  , C.KnownNat (ARAddrWidth conf)

  , Show (RegionType (ARKeepRegion conf))
  , Show (BurstLengthType (ARKeepBurstLength conf))
  , Show (SizeType (ARKeepSize conf))
  , Show (BurstType (ARKeepBurst conf))
  , Show (LockType (ARKeepLock conf))
  , Show (CacheType (ARKeepCache conf))
  , Show (PermissionsType (ARKeepPermissions conf))
  , Show (QosType (ARKeepQos conf))

  , C.NFDataX (RegionType (ARKeepRegion conf))
  , C.NFDataX (BurstLengthType (ARKeepBurstLength conf))
  , C.NFDataX (SizeType (ARKeepSize conf))
  , C.NFDataX (BurstType (ARKeepBurst conf))
  , C.NFDataX (LockType (ARKeepLock conf))
  , C.NFDataX (CacheType (ARKeepCache conf))
  , C.NFDataX (PermissionsType (ARKeepPermissions conf))
  , C.NFDataX (QosType (ARKeepQos conf))
  ) => GoodAxi4ReadAddressConfig conf

instance
  ( KeepTypeClass (ARKeepBurst conf)
  , KeepTypeClass (ARKeepSize conf)
  , KeepTypeClass (ARKeepRegion conf)
  , KeepTypeClass (ARKeepBurstLength conf)
  , KeepTypeClass (ARKeepLock conf)
  , KeepTypeClass (ARKeepCache conf)
  , KeepTypeClass (ARKeepPermissions conf)
  , KeepTypeClass (ARKeepQos conf)

  , C.KnownNat (ARIdWidth conf)
  , C.KnownNat (ARAddrWidth conf)

  , Show (RegionType (ARKeepRegion conf))
  , Show (BurstLengthType (ARKeepBurstLength conf))
  , Show (SizeType (ARKeepSize conf))
  , Show (BurstType (ARKeepBurst conf))
  , Show (LockType (ARKeepLock conf))
  , Show (CacheType (ARKeepCache conf))
  , Show (PermissionsType (ARKeepPermissions conf))
  , Show (QosType (ARKeepQos conf))

  , C.NFDataX (RegionType (ARKeepRegion conf))
  , C.NFDataX (BurstLengthType (ARKeepBurstLength conf))
  , C.NFDataX (SizeType (ARKeepSize conf))
  , C.NFDataX (BurstType (ARKeepBurst conf))
  , C.NFDataX (LockType (ARKeepLock conf))
  , C.NFDataX (CacheType (ARKeepCache conf))
  , C.NFDataX (PermissionsType (ARKeepPermissions conf))
  , C.NFDataX (QosType (ARKeepQos conf))
  ) => GoodAxi4ReadAddressConfig conf

deriving instance
  ( GoodAxi4ReadAddressConfig conf
  , Show userType
  ) =>
  Show (M2S_ReadAddress conf userType)

deriving instance
  ( GoodAxi4ReadAddressConfig conf
  , C.NFDataX userType
  ) =>
  C.NFDataX (M2S_ReadAddress conf userType)

-- | Circuit that transforms the LHS 'Axi4ReadAddress' protocol to a
-- version using different type parameters according to two functions
-- that can transform the data and ack signal to and from the other protocol.
mapFull ::
  forall dom
    conf1 t1
    conf2 t2 .
  (M2S_ReadAddress conf1 t1 ->
    M2S_ReadAddress conf2 t2) ->
  (S2M_ReadAddress -> S2M_ReadAddress) ->
  Circuit
    (Axi4ReadAddress dom conf1 t1)
    (Axi4ReadAddress dom conf2 t2)
mapFull = DfLike.mapDfLike Proxy Proxy
