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

module Protocols.Axi4.ReadAddress
  ( M2S_ReadAddress(..)
  , S2M_ReadAddress(..)
  , Axi4ReadAddress

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

    -- * read address info
  , Axi4ReadAddressInfo(..)
  , axi4ReadAddrMsgToReadAddrInfo
  , axi4ReadAddrMsgFromReadAddrInfo
  ) where

-- base
import Data.Coerce
import Data.Kind (Type)
import GHC.Generics (Generic)

-- clash-prelude
import qualified Clash.Prelude as C

-- me
import Protocols.Axi4.Common
import Protocols.Internal

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

-- | Mainly for use in @DfConv@.
--
-- Data carried along 'Axi4ReadAddress' channel which is put in control of
-- the user, rather than being managed by the @DfConv@ instances. Matches up
-- one-to-one with the fields of 'M2S_ReadAddress' except for '_arlen',
-- '_arsize', and '_arburst'.
data Axi4ReadAddressInfo (conf :: Axi4ReadAddressConfig) (userType :: Type)
  = Axi4ReadAddressInfo
  { -- | Id
    _ariid :: !(C.BitVector (ARIdWidth conf))

    -- | Address
  , _ariaddr :: !(C.BitVector (ARAddrWidth conf))

    -- | Region
  , _ariregion :: !(RegionType (ARKeepRegion conf))

    -- | Burst length
  , _arilen :: !(BurstLengthType (ARKeepBurstLength conf))

    -- | Burst size
  , _arisize :: !(SizeType (ARKeepSize conf))

    -- | Burst type
  , _ariburst :: !(BurstType (ARKeepBurst conf))

    -- | Lock type
  , _arilock :: !(LockType (ARKeepLock conf))

    -- | Cache type
  , _aricache :: !(CacheType (ARKeepCache conf))

    -- | Protection type
  , _ariprot :: !(PermissionsType (ARKeepPermissions conf))

    -- | QoS value
  , _ariqos :: !(QosType (ARKeepQos conf))

    -- | User data
  , _ariuser :: !userType
  }
  deriving (Generic)

deriving instance
  ( GoodAxi4ReadAddressConfig conf
  , Show userType ) =>
  Show (Axi4ReadAddressInfo conf userType)

deriving instance
  ( GoodAxi4ReadAddressConfig conf
  , C.NFDataX userType ) =>
  C.NFDataX (Axi4ReadAddressInfo conf userType)

-- | Convert 'M2S_ReadAddress' to 'Axi4ReadAddressInfo', dropping some info
axi4ReadAddrMsgToReadAddrInfo
  :: M2S_ReadAddress conf userType
  -> Axi4ReadAddressInfo conf userType
axi4ReadAddrMsgToReadAddrInfo M2S_NoReadAddress = C.errorX "Expected ReadAddress"
axi4ReadAddrMsgToReadAddrInfo M2S_ReadAddress{..}
  = Axi4ReadAddressInfo
  { _ariid     = _arid
  , _ariaddr   = _araddr
  , _ariregion = _arregion
  , _arilen    = _arlen
  , _arisize   = _arsize
  , _ariburst  = _arburst
  , _arilock   = _arlock
  , _aricache  = _arcache
  , _ariprot   = _arprot
  , _ariqos    = _arqos
  , _ariuser   = _aruser
  }

-- | Convert 'Axi4ReadAddressInfo' to 'M2S_ReadAddress', adding some info
axi4ReadAddrMsgFromReadAddrInfo
  :: Axi4ReadAddressInfo conf userType -> M2S_ReadAddress conf userType
axi4ReadAddrMsgFromReadAddrInfo Axi4ReadAddressInfo{..}
  = M2S_ReadAddress
  { _arid     = _ariid
  , _araddr   = _ariaddr
  , _arregion = _ariregion
  , _arlen    = _arilen
  , _arsize   = _arisize
  , _arburst  = _ariburst
  , _arlock   = _arilock
  , _arcache  = _aricache
  , _arprot   = _ariprot
  , _arqos    = _ariqos
  , _aruser   = _ariuser
  }
