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

module Protocols.Axi4.WriteAddress
  ( M2S_WriteAddress(..)
  , S2M_WriteAddress(..)
  , Axi4WriteAddress

    -- * configuration
  , Axi4WriteAddressConfig(..)
  , GoodAxi4WriteAddressConfig
  , AWKeepBurst
  , AWKeepSize
  , AWIdWidth
  , AWAddrWidth
  , AWKeepRegion
  , AWKeepBurstLength
  , AWKeepLock
  , AWKeepCache
  , AWKeepPermissions
  , AWKeepQos
  ) where

-- base
import Data.Coerce (coerce)
import Data.Kind (Type)
import GHC.Generics (Generic)

-- clash-prelude
import qualified Clash.Prelude as C

-- me
import Protocols.Axi4.Common
import Protocols.Internal

-- | Configuration options for 'Axi4WriteAddress'.
data Axi4WriteAddressConfig = Axi4WriteAddressConfig
  { _awKeepBurst       :: Bool
  , _awKeepSize        :: Bool
  , _awIdWidth         :: C.Nat
  , _awAddrWidth       :: C.Nat
  , _awKeepRegion      :: Bool
  , _awKeepBurstLength :: Bool
  , _awKeepLock        :: Bool
  , _awKeepCache       :: Bool
  , _awKeepPermissions :: Bool
  , _awKeepQos         :: Bool
  }

-- | Grab '_awKeepBurst' from 'Axi4WriteAddressConfig' at the type level.
-- This boolean value determines whether to keep the '_awburst' field
-- in 'M2S_WriteAddress'.
type family AWKeepBurst (c :: Axi4WriteAddressConfig) where
  AWKeepBurst ('Axi4WriteAddressConfig a _ _ _ _ _ _ _ _ _) = a

-- | Grab '_awKeepSize' from 'Axi4WriteAddressConfig' at the type level.
-- This boolean value determines whether to keep the '_awsize' field
-- in 'M2S_WriteAddress'.
type family AWKeepSize (c :: Axi4WriteAddressConfig) where
  AWKeepSize ('Axi4WriteAddressConfig _ a _ _ _ _ _ _ _ _) = a

-- | Grab '_awIdWidth' from 'Axi4WriteAddressConfig' at the type level.
-- This nat value determines the size of the '_awid' field
-- in 'M2S_WriteAddress'.
type family AWIdWidth (c :: Axi4WriteAddressConfig) where
  AWIdWidth ('Axi4WriteAddressConfig _ _ a _ _ _ _ _ _ _) = a

-- | Grab '_awAddrWidth' from 'Axi4WriteAddressConfig' at the type level.
-- This nat value determines the size of the '_awaddr' field
-- in 'M2S_WriteAddress'.
type family AWAddrWidth (c :: Axi4WriteAddressConfig) where
  AWAddrWidth ('Axi4WriteAddressConfig _ _ _ a _ _ _ _ _ _) = a

-- | Grab '_awKeepRegion' from 'Axi4WriteAddressConfig' at the type level.
-- This boolean value determines whether to keep the '_awregion' field
-- in 'M2S_WriteAddress'.
type family AWKeepRegion (c :: Axi4WriteAddressConfig) where
  AWKeepRegion ('Axi4WriteAddressConfig _ _ _ _ a _ _ _ _ _) = a

-- | Grab '_awKeepBurstLength' from 'Axi4WriteAddressConfig' at the type level.
-- This boolean value determines whether to keep the '_awlen' field
-- in 'M2S_WriteAddress'.
type family AWKeepBurstLength (c :: Axi4WriteAddressConfig) where
  AWKeepBurstLength ('Axi4WriteAddressConfig _ _ _ _ _ a _ _ _ _) = a

-- | Grab '_awKeepLock' from 'Axi4WriteAddressConfig' at the type level.
-- This boolean value determines whether to keep the '_awlock' field
-- in 'M2S_WriteAddress'.
type family AWKeepLock (c :: Axi4WriteAddressConfig) where
  AWKeepLock ('Axi4WriteAddressConfig _ _ _ _ _ _ a _ _ _) = a

-- | Grab '_awKeepCache' from 'Axi4WriteAddressConfig' at the type level.
-- This boolean value determines whether to keep the '_awcache' field
-- in 'M2S_WriteAddress'.
type family AWKeepCache (c :: Axi4WriteAddressConfig) where
  AWKeepCache ('Axi4WriteAddressConfig _ _ _ _ _ _ _ a _ _) = a

-- | Grab '_awKeepPermissions' from 'Axi4WriteAddressConfig' at the type level.
-- This boolean value determines whether to keep the '_awprot' field
-- in 'M2S_WriteAddress'.
type family AWKeepPermissions (c :: Axi4WriteAddressConfig) where
  AWKeepPermissions ('Axi4WriteAddressConfig _ _ _ _ _ _ _ _ a _) = a

-- | Grab '_awKeepQos' from 'Axi4WriteAddressConfig' at the type level.
-- This boolean value determines whether to keep the '_awqos' field
-- in 'M2S_WriteAddress'.
type family AWKeepQos (c :: Axi4WriteAddressConfig) where
  AWKeepQos ('Axi4WriteAddressConfig _ _ _ _ _ _ _ _ _ a) = a

-- | AXI4 Write Address channel protocol
data Axi4WriteAddress
  (dom :: C.Domain)
  (conf :: Axi4WriteAddressConfig)
  (userType :: Type)

instance Protocol (Axi4WriteAddress dom conf userType) where
  type Fwd (Axi4WriteAddress dom conf userType) =
    C.Signal dom (M2S_WriteAddress conf userType)
  type Bwd (Axi4WriteAddress dom conf userType) =
    C.Signal dom S2M_WriteAddress

instance Backpressure (Axi4WriteAddress dom conf userType) where
  boolsToBwd _ = C.fromList_lazy . coerce


-- | See Table A2-2 "Write address channel signals"
data M2S_WriteAddress
  (conf :: Axi4WriteAddressConfig)
  (userType :: Type)
  = M2S_NoWriteAddress
  | M2S_WriteAddress
    { -- | Write address id*
      _awid :: !(C.BitVector (AWIdWidth conf))

      -- | Write address
    , _awaddr :: !(C.BitVector (AWAddrWidth conf))

      -- | Write region*
    , _awregion:: !(RegionType (AWKeepRegion conf))

      -- | Burst length*
    , _awlen :: !(BurstLengthType (AWKeepBurstLength conf))

      -- | Burst size*
    , _awsize :: !(SizeType (AWKeepSize conf))

      -- | Burst type*
    , _awburst :: !(BurstType (AWKeepBurst conf))

      -- | Lock type*
    , _awlock :: !(LockType (AWKeepLock conf))

      -- | Cache type*
    , _awcache :: !(CacheType (AWKeepCache conf))

      -- | Protection type
    , _awprot :: !(PermissionsType (AWKeepPermissions conf))

      -- | QoS value
    , _awqos :: !(QosType (AWKeepQos conf))

      -- | User data
    , _awuser :: !userType
    }
  deriving (Generic)

-- | See Table A2-2 "Write address channel signals"
newtype S2M_WriteAddress = S2M_WriteAddress { _awready :: Bool }
  deriving (Show, Generic, C.NFDataX)

-- | Shorthand for a "well-behaved" write address config,
-- so that we don't need to write out a bunch of type constraints later.
-- Holds for every configuration; don't worry about implementing this class.
class
  ( KeepTypeClass (AWKeepBurst conf)
  , KeepTypeClass (AWKeepSize conf)
  , KeepTypeClass (AWKeepRegion conf)
  , KeepTypeClass (AWKeepBurstLength conf)
  , KeepTypeClass (AWKeepLock conf)
  , KeepTypeClass (AWKeepCache conf)
  , KeepTypeClass (AWKeepPermissions conf)
  , KeepTypeClass (AWKeepQos conf)

  , C.KnownNat (AWIdWidth conf)
  , C.KnownNat (AWAddrWidth conf)

  , Show (RegionType (AWKeepRegion conf))
  , Show (BurstLengthType (AWKeepBurstLength conf))
  , Show (SizeType (AWKeepSize conf))
  , Show (BurstType (AWKeepBurst conf))
  , Show (LockType (AWKeepLock conf))
  , Show (CacheType (AWKeepCache conf))
  , Show (PermissionsType (AWKeepPermissions conf))
  , Show (QosType (AWKeepQos conf))

  , C.NFDataX (RegionType (AWKeepRegion conf))
  , C.NFDataX (BurstLengthType (AWKeepBurstLength conf))
  , C.NFDataX (SizeType (AWKeepSize conf))
  , C.NFDataX (BurstType (AWKeepBurst conf))
  , C.NFDataX (LockType (AWKeepLock conf))
  , C.NFDataX (CacheType (AWKeepCache conf))
  , C.NFDataX (PermissionsType (AWKeepPermissions conf))
  , C.NFDataX (QosType (AWKeepQos conf))
  ) => GoodAxi4WriteAddressConfig conf

instance
  ( KeepTypeClass (AWKeepBurst conf)
  , KeepTypeClass (AWKeepSize conf)
  , KeepTypeClass (AWKeepRegion conf)
  , KeepTypeClass (AWKeepBurstLength conf)
  , KeepTypeClass (AWKeepLock conf)
  , KeepTypeClass (AWKeepCache conf)
  , KeepTypeClass (AWKeepPermissions conf)
  , KeepTypeClass (AWKeepQos conf)

  , C.KnownNat (AWIdWidth conf)
  , C.KnownNat (AWAddrWidth conf)

  , Show (RegionType (AWKeepRegion conf))
  , Show (BurstLengthType (AWKeepBurstLength conf))
  , Show (SizeType (AWKeepSize conf))
  , Show (BurstType (AWKeepBurst conf))
  , Show (LockType (AWKeepLock conf))
  , Show (CacheType (AWKeepCache conf))
  , Show (PermissionsType (AWKeepPermissions conf))
  , Show (QosType (AWKeepQos conf))

  , C.NFDataX (RegionType (AWKeepRegion conf))
  , C.NFDataX (BurstLengthType (AWKeepBurstLength conf))
  , C.NFDataX (SizeType (AWKeepSize conf))
  , C.NFDataX (BurstType (AWKeepBurst conf))
  , C.NFDataX (LockType (AWKeepLock conf))
  , C.NFDataX (CacheType (AWKeepCache conf))
  , C.NFDataX (PermissionsType (AWKeepPermissions conf))
  , C.NFDataX (QosType (AWKeepQos conf))
  ) => GoodAxi4WriteAddressConfig conf

deriving instance
  ( GoodAxi4WriteAddressConfig conf
  , Show userType
  ) =>
  Show (M2S_WriteAddress conf userType)

deriving instance
  ( GoodAxi4WriteAddressConfig conf
  , C.NFDataX userType
  ) =>
  C.NFDataX (M2S_WriteAddress conf userType)
