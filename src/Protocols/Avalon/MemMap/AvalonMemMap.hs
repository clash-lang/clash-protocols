{-|
Types and instance declarations for the Avalon memory mapped protocol
(http://www1.cs.columbia.edu/~sedwards/classes/2009/4840/mnl_avalon_spec.pdf).
Non-required fields can be easily toggled by the user. The @data@ and
@outputenable@ fields are not supported since we would need bidirectional data
ports. The @resetrequest@ field is also not supported since this does not get
transferred around, but rather gets send "outwards" to whoever is controlling
the reset signal of the circuit.
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Protocols.Avalon.MemMap.AvalonMemMap
  ( -- * Configuration types
    AvalonMMSharedConfig(..)
  , AvalonMMSubordinateConfig(..)
  , AvalonMMManagerConfig(..)

    -- * Grab members from AvalonMMSharedConfig at the type level
  , DataWidth
  , KeepReadData
  , KeepWriteData
  , AddrWidth
  , KeepRead
  , KeepWrite
  , ByteEnableWidth
  , KeepByteEnable
  , BurstCountWidth
  , KeepBurstCount
  , KeepReadDataValid
  , KeepEndOfPacket

    -- * Grab members from AvalonMMSubordinateConfig at the type level
  , KeepAddr
  , KeepWriteByteEnable
  , KeepChipSelect
  , KeepBeginTransfer
  , KeepWaitRequest
  , KeepBeginBurstTransfer
  , KeepReadyForData
  , KeepDataAvailable
  , KeepIrq
  , SShared

    -- * Grab members from AvalonMMManagerConfig at the type level
  , KeepFlush
  , KeepIrqList
  , KeepIrqNumber
  , MShared

    -- * Remove DfConv-incompatible fields from configs
  , RemoveNonDfSubordinate
  , RemoveNonDfManager

    -- * Constraints on configs
  , KnownMMSharedConfig
  , KnownMMSubordinateConfig
  , KnownMMManagerConfig

    -- * Avalon MM signals
  , AvalonManagerOut(..)
  , AvalonManagerIn(..)
  , AvalonSubordinateOut(..)
  , AvalonSubordinateIn(..)

    -- * Important parts of signals, used as payload in DfConv
  , AvalonWriteImpt(..)
  , AvalonReadReqImpt(..)
  , AvalonReadImpt(..)

    -- * Helper functions
  , managerOutAddNonDf
  , managerOutRemoveNonDf
  , managerInAddNonDf
  , managerInRemoveNonDf
  , subordinateOutAddNonDf
  , subordinateOutRemoveNonDf
  , subordinateInAddNonDf
  , subordinateInRemoveNonDf

    -- * Protocols
  , AvalonMMManager(..)
  , AvalonMMSubordinate(..)
  ) where

-- base
import Prelude ()

import           Control.Monad.State (put, get)
import           Control.DeepSeq (NFData)
import qualified Data.Maybe as Maybe
import           Data.Proxy

-- clash-prelude
import           Clash.Prelude hiding (take, concat, length)
import qualified Clash.Prelude as C

-- me
import           Protocols.Internal
import qualified Protocols.DfConv as DfConv


-- | Config needed for both manager and subordinate interfaces.
-- @Bool@ values represent whether to keep a boolean field or not.
-- @Nat@ values represent the width of a variable-sized numeric field.
data AvalonMMSharedConfig
  =  AvalonMMSharedConfig
  { dataWidth         :: Nat
  , keepReadData      :: Bool
  , keepWriteData     :: Bool
  , addrWidth         :: Nat
  , keepRead          :: Bool
  , keepWrite         :: Bool
  , byteEnableWidth   :: Nat
  , keepByteEnable    :: Bool
  , burstCountWidth   :: Nat
  , keepBurstCount    :: Bool
  , keepReadDataValid :: Bool
  , keepEndOfPacket   :: Bool
  }

-- | Config specific to Avalon MM subordinate interfaces.
-- @Bool@ values represent whether to keep a boolean field or not.
-- @Nat@ values represent the width of a variable-sized numeric field.
-- An @AvalonMMSharedConfig@ is also included for the rest of the fields.
data AvalonMMSubordinateConfig
  =  AvalonMMSubordinateConfig
  { keepAddr               :: Bool
  , keepWriteByteEnable    :: Bool
  , keepChipSelect         :: Bool
  , keepBeginTransfer      :: Bool
  , keepWaitRequest        :: Bool
  , keepBeginBurstTransfer :: Bool
  , keepReadyForData       :: Bool
  , keepDataAvailable      :: Bool
  , keepIrq                :: Bool
  , sShared                :: AvalonMMSharedConfig
  }

-- | Config specific to Avalon MM manager interfaces.
-- @Bool@ values represent whether to keep a boolean field or not.
-- @Nat@ values represent the width of a variable-sized numeric field.
-- An @AvalonMMSharedConfig@ is also included for the rest of the fields.
data AvalonMMManagerConfig
  =  AvalonMMManagerConfig
  { keepFlush      :: Bool
  , keepIrqList    :: Bool
  , keepIrqNumber  :: Bool
  , mShared        :: AvalonMMSharedConfig
  }

-- Grab record fields at the type level:

-- | Grab 'dataWidth' from 'AvalonMMSharedConfig' at the type level
type family DataWidth (c :: AvalonMMSharedConfig) where
  DataWidth ('AvalonMMSharedConfig a _ _ _ _ _ _ _ _ _ _ _) = a

-- | Grab 'keepReadData' from 'AvalonMMSharedConfig' at the type level
type family KeepReadData (c :: AvalonMMSharedConfig) where
  KeepReadData ('AvalonMMSharedConfig _ a _ _ _ _ _ _ _ _ _ _) = a

-- | Grab 'keepWriteData' from 'AvalonMMSharedConfig' at the type level
type family KeepWriteData (c :: AvalonMMSharedConfig) where
  KeepWriteData ('AvalonMMSharedConfig _ _ a _ _ _ _ _ _ _ _ _) = a

-- | Grab 'addrWidth' from 'AvalonMMSharedConfig' at the type level
type family AddrWidth (c :: AvalonMMSharedConfig) where
  AddrWidth ('AvalonMMSharedConfig _ _ _ a _ _ _ _ _ _ _ _) = a

-- | Grab 'keepRead' from 'AvalonMMSharedConfig' at the type level
type family KeepRead (c :: AvalonMMSharedConfig) where
  KeepRead ('AvalonMMSharedConfig _ _ _ _ a _ _ _ _ _ _ _) = a

-- | Grab 'keepWrite' from 'AvalonMMSharedConfig' at the type level
type family KeepWrite (c :: AvalonMMSharedConfig) where
  KeepWrite ('AvalonMMSharedConfig _ _ _ _ _ a _ _ _ _ _ _) = a

-- | Grab 'byteEnableWidth' from 'AvalonMMSharedConfig' at the type level
type family ByteEnableWidth (c :: AvalonMMSharedConfig) where
  ByteEnableWidth ('AvalonMMSharedConfig _ _ _ _ _ _ a _ _ _ _ _) = a

-- | Grab 'keepByteEnable' from 'AvalonMMSharedConfig' at the type level
type family KeepByteEnable (c :: AvalonMMSharedConfig) where
  KeepByteEnable ('AvalonMMSharedConfig _ _ _ _ _ _ _ a _ _ _ _) = a

-- | Grab 'burstCountWidth' from 'AvalonMMSharedConfig' at the type level
type family BurstCountWidth (c :: AvalonMMSharedConfig) where
  BurstCountWidth ('AvalonMMSharedConfig _ _ _ _ _ _ _ _ a _ _ _) = a

-- | Grab 'keepBurstCount' from 'AvalonMMSharedConfig' at the type level
type family KeepBurstCount (c :: AvalonMMSharedConfig) where
  KeepBurstCount ('AvalonMMSharedConfig _ _ _ _ _ _ _ _ _ a _ _) = a

-- | Grab 'keepReadDataValid' from 'AvalonMMSharedConfig' at the type level
type family KeepReadDataValid (c :: AvalonMMSharedConfig) where
  KeepReadDataValid ('AvalonMMSharedConfig _ _ _ _ _ _ _ _ _ _ a _) = a

-- | Grab 'keepEndOfPacket' from 'AvalonMMSharedConfig' at the type level
type family KeepEndOfPacket (c :: AvalonMMSharedConfig) where
  KeepEndOfPacket ('AvalonMMSharedConfig _ _ _ _ _ _ _ _ _ _ _ a) = a


-- | Grab 'keepAddr' from 'AvalonMMSubordinateConfig' at the type level
type family KeepAddr (c :: AvalonMMSubordinateConfig) where
  KeepAddr ('AvalonMMSubordinateConfig a _ _ _ _ _ _ _ _ _) = a

-- | Grab 'keepWriteByteEnable' from 'AvalonMMSubordinateConfig' at the type level
type family KeepWriteByteEnable (c :: AvalonMMSubordinateConfig) where
  KeepWriteByteEnable ('AvalonMMSubordinateConfig _ a _ _ _ _ _ _ _ _) = a

-- | Grab 'keepChipSelect' from 'AvalonMMSubordinateConfig' at the type level
type family KeepChipSelect (c :: AvalonMMSubordinateConfig) where
  KeepChipSelect ('AvalonMMSubordinateConfig _ _ a _ _ _ _ _ _ _) = a

-- | Grab 'keepBeginTransfer' from 'AvalonMMSubordinateConfig' at the type level
type family KeepBeginTransfer (c :: AvalonMMSubordinateConfig) where
  KeepBeginTransfer ('AvalonMMSubordinateConfig _ _ _ a _ _ _ _ _ _) = a

-- | Grab 'keepWaitRequest' from 'AvalonMMSubordinateConfig' at the type level
type family KeepWaitRequest (c :: AvalonMMSubordinateConfig) where
  KeepWaitRequest ('AvalonMMSubordinateConfig _ _ _ _ a _ _ _ _ _) = a

-- | Grab 'keepBeginBurstTransfer' from 'AvalonMMSubordinateConfig' at the type level
type family KeepBeginBurstTransfer (c :: AvalonMMSubordinateConfig) where
  KeepBeginBurstTransfer ('AvalonMMSubordinateConfig _ _ _ _ _ a _ _ _ _) = a

-- | Grab 'keepReadyForData' from 'AvalonMMSubordinateConfig' at the type level
type family KeepReadyForData (c :: AvalonMMSubordinateConfig) where
  KeepReadyForData ('AvalonMMSubordinateConfig _ _ _ _ _ _ a _ _ _) = a

-- | Grab 'keepDataAvailable' from 'AvalonMMSubordinateConfig' at the type level
type family KeepDataAvailable (c :: AvalonMMSubordinateConfig) where
  KeepDataAvailable ('AvalonMMSubordinateConfig _ _ _ _ _ _ _ a _ _) = a

-- | Grab 'keepIrq' from 'AvalonMMSubordinateConfig' at the type level
type family KeepIrq (c :: AvalonMMSubordinateConfig) where
  KeepIrq ('AvalonMMSubordinateConfig _ _ _ _ _ _ _ _ a _) = a

-- | Grab 'sShared' from 'AvalonMMSubordinateConfig' at the type level
type family SShared (c :: AvalonMMSubordinateConfig) where
  SShared ('AvalonMMSubordinateConfig _ _ _ _ _ _ _ _ _ a) = a


-- | Grab 'keepFlush' from 'AvalonMMManagerConfig' at the type level
type family KeepFlush (c :: AvalonMMManagerConfig) where
  KeepFlush ('AvalonMMManagerConfig a _ _ _) = a

-- | Grab 'keepIrqList' from 'AvalonMMManagerConfig' at the type level
type family KeepIrqList (c :: AvalonMMManagerConfig) where
  KeepIrqList ('AvalonMMManagerConfig _ a _ _) = a

-- | Grab 'keepIrqNumber' from 'AvalonMMManagerConfig' at the type level
type family KeepIrqNumber (c :: AvalonMMManagerConfig) where
  KeepIrqNumber ('AvalonMMManagerConfig _ _ a _) = a

-- | Grab 'mShared' from 'AvalonMMManagerConfig' at the type level
type family MShared (c :: AvalonMMManagerConfig) where
  MShared ('AvalonMMManagerConfig _ _ _ a) = a


-- | Disable fields of 'AvalonMMSubordinateConfig' that are not allowed in the
-- 'DfConv.DfConv' instance
type family RemoveNonDfSubordinate (cfg :: AvalonMMSubordinateConfig) where
  RemoveNonDfSubordinate cfg
    = 'AvalonMMSubordinateConfig
      (KeepAddr cfg)
      (KeepWriteByteEnable cfg)
      (KeepChipSelect cfg)
      'False
      (KeepWaitRequest cfg)
      'False
      'False
      'False
      'False
      (SShared cfg)

-- | Disable fields of 'AvalonMMManagerConfig' that are not allowed in the
-- 'DfConv.DfConv' instance
type family RemoveNonDfManager (cfg :: AvalonMMManagerConfig) where
  RemoveNonDfManager cfg
    = 'AvalonMMManagerConfig
      'False
      'False
      'False
      (MShared cfg)


-- | Constraint representing a well-behaved shared config.
-- This class holds for every possible @AvalonMMSharedConfig@,
-- but we need to write out the class anyway so that GHC holds.
type KnownMMSharedConfig config =
  ( KnownNat      (DataWidth         config)
  , KeepTypeClass (KeepReadData      config)
  , KeepTypeClass (KeepWriteData     config)
  , KnownNat      (AddrWidth         config)
  , KeepTypeClass (KeepRead          config)
  , KeepTypeClass (KeepWrite         config)
  , KnownNat      (ByteEnableWidth   config)
  , KeepTypeClass (KeepByteEnable    config)
  , KnownNat      (BurstCountWidth   config)
  , KeepTypeClass (KeepBurstCount    config)
  , KeepTypeClass (KeepReadDataValid config)
  , KeepTypeClass (KeepEndOfPacket   config)

  , NFDataX (KeepType (KeepReadData config) (Unsigned (DataWidth config)))
  , NFData  (KeepType (KeepReadData config) (Unsigned (DataWidth config)))
  , ShowX   (KeepType (KeepReadData config) (Unsigned (DataWidth config)))
  , Show    (KeepType (KeepReadData config) (Unsigned (DataWidth config)))
  , Eq      (KeepType (KeepReadData config) (Unsigned (DataWidth config)))

  , NFDataX (KeepType (KeepWriteData config) (Unsigned (DataWidth config)))
  , NFData  (KeepType (KeepWriteData config) (Unsigned (DataWidth config)))
  , ShowX   (KeepType (KeepWriteData config) (Unsigned (DataWidth config)))
  , Show    (KeepType (KeepWriteData config) (Unsigned (DataWidth config)))
  , Eq      (KeepType (KeepWriteData config) (Unsigned (DataWidth config)))

  , NFDataX (KeepType (KeepByteEnable config) (Unsigned (ByteEnableWidth config)))
  , NFData  (KeepType (KeepByteEnable config) (Unsigned (ByteEnableWidth config)))
  , ShowX   (KeepType (KeepByteEnable config) (Unsigned (ByteEnableWidth config)))
  , Show    (KeepType (KeepByteEnable config) (Unsigned (ByteEnableWidth config)))
  , Eq      (KeepType (KeepByteEnable config) (Unsigned (ByteEnableWidth config)))

  , NFDataX (KeepType (KeepBurstCount config) (Unsigned (BurstCountWidth config)))
  , NFData  (KeepType (KeepBurstCount config) (Unsigned (BurstCountWidth config)))
  , ShowX   (KeepType (KeepBurstCount config) (Unsigned (BurstCountWidth config)))
  , Show    (KeepType (KeepBurstCount config) (Unsigned (BurstCountWidth config)))
  , Eq      (KeepType (KeepBurstCount config) (Unsigned (BurstCountWidth config)))
  )

-- | Constraint representing a well-behaved subordinate config.
-- This class holds for every possible @AvalonMMSubordinateConfig@,
-- but we need to write out the class anyway so that GHC holds.
type KnownMMSubordinateConfig config =
  ( KeepTypeClass (KeepAddr               config)
  , KeepTypeClass (KeepWriteByteEnable    config)
  , KeepTypeClass (KeepChipSelect         config)
  , KeepTypeClass (KeepBeginTransfer      config)
  , KeepTypeClass (KeepWaitRequest        config)
  , KeepTypeClass (KeepBeginBurstTransfer config)
  , KeepTypeClass (KeepReadyForData       config)
  , KeepTypeClass (KeepDataAvailable      config)
  , KeepTypeClass (KeepIrq                config)
  , KnownMMSharedConfig (SShared           config)

  , NFDataX (KeepType (KeepAddr config) (Unsigned (AddrWidth (SShared config))))
  , NFData  (KeepType (KeepAddr config) (Unsigned (AddrWidth (SShared config))))
  , ShowX   (KeepType (KeepAddr config) (Unsigned (AddrWidth (SShared config))))
  , Show    (KeepType (KeepAddr config) (Unsigned (AddrWidth (SShared config))))
  , Eq      (KeepType (KeepAddr config) (Unsigned (AddrWidth (SShared config))))

  , NFDataX (KeepType (KeepWriteByteEnable config) (Unsigned (ByteEnableWidth (SShared config))))
  , NFData  (KeepType (KeepWriteByteEnable config) (Unsigned (ByteEnableWidth (SShared config))))
  , ShowX   (KeepType (KeepWriteByteEnable config) (Unsigned (ByteEnableWidth (SShared config))))
  , Show    (KeepType (KeepWriteByteEnable config) (Unsigned (ByteEnableWidth (SShared config))))
  , Eq      (KeepType (KeepWriteByteEnable config) (Unsigned (ByteEnableWidth (SShared config))))
  )

-- | Constraint representing a well-behaved manager config.
-- This class holds for every possible @AvalonMMManagerConfig@,
-- but we need to write out the class anyway so that GHC holds.
type KnownMMManagerConfig config =
  ( KeepTypeClass (KeepFlush     config)
  , KeepTypeClass (KeepIrqList   config)
  , KeepTypeClass (KeepIrqNumber config)
  , KnownMMSharedConfig (MShared config)

  , NFDataX (KeepType (KeepIrqList config) (Unsigned 32))
  , NFData  (KeepType (KeepIrqList config) (Unsigned 32))
  , ShowX   (KeepType (KeepIrqList config) (Unsigned 32))
  , Show    (KeepType (KeepIrqList config) (Unsigned 32))
  , Eq      (KeepType (KeepIrqList config) (Unsigned 32))

  , NFDataX (KeepType (KeepIrqNumber config) (Maybe (Unsigned 6)))
  , NFData  (KeepType (KeepIrqNumber config) (Maybe (Unsigned 6)))
  , ShowX   (KeepType (KeepIrqNumber config) (Maybe (Unsigned 6)))
  , Show    (KeepType (KeepIrqNumber config) (Maybe (Unsigned 6)))
  , Eq      (KeepType (KeepIrqNumber config) (Maybe (Unsigned 6)))
  )


-- | Data coming out of an Avalon MM manager port.
-- All fields are optional and can be toggled using the config.
data AvalonManagerOut config
  = AvalonManagerOut
  { mo_writeData :: KeepType (KeepWriteData (MShared config))
                      (Unsigned (DataWidth (MShared config)))
  , mo_addr :: Unsigned (AddrWidth (MShared config))
  , mo_read :: KeepType (KeepRead (MShared config)) Bool
  , mo_write :: KeepType (KeepWrite (MShared config)) Bool
  , mo_byteEnable :: KeepType (KeepByteEnable (MShared config))
                       (Unsigned (ByteEnableWidth (MShared config)))
  , mo_burstCount :: KeepType (KeepBurstCount (MShared config))
                       (Unsigned (BurstCountWidth (MShared config)))
  , mo_flush :: KeepType (KeepFlush config) Bool
  }
  deriving (Generic, Bundle)

deriving instance (KnownMMManagerConfig config)
                   => NFDataX (AvalonManagerOut config)
deriving instance (KnownMMManagerConfig config)
                   => NFData (AvalonManagerOut config)
deriving instance (KnownMMManagerConfig config)
                   => ShowX (AvalonManagerOut config)
deriving instance (KnownMMManagerConfig config)
                   => Show (AvalonManagerOut config)
deriving instance (KnownMMManagerConfig config)
                   => Eq (AvalonManagerOut config)


-- | Data coming into an Avalon MM manager port.
-- Almost all fields are optional and can be toggled using the config.
-- WaitRequest is mandatory.
data AvalonManagerIn config
   =  AvalonManagerIn
  { mi_readData :: KeepType (KeepReadData (MShared config))
                     (Unsigned (DataWidth (MShared config)))
  , mi_waitRequest :: Bool
  , mi_readDataValid :: KeepType (KeepReadDataValid (MShared config)) Bool
  , mi_endOfPacket :: KeepType (KeepEndOfPacket (MShared config)) Bool
  , mi_irqList :: KeepType (KeepIrqList config) (Unsigned 32)
  , mi_irqNumber :: KeepType (KeepIrqNumber config) (Maybe (Unsigned 6))
  }
  deriving (Generic, Bundle)

deriving instance (KnownMMManagerConfig config)
                   => NFDataX (AvalonManagerIn config)
deriving instance (KnownMMManagerConfig config)
                   => NFData (AvalonManagerIn config)
deriving instance (KnownMMManagerConfig config)
                   => ShowX (AvalonManagerIn config)
deriving instance (KnownMMManagerConfig config)
                   => Show (AvalonManagerIn config)
deriving instance (KnownMMManagerConfig config)
                   => Eq (AvalonManagerIn config)


-- | Data coming out of an Avalon MM subordinate port.
-- All fields are optional and can be toggled using the config.
data AvalonSubordinateOut config
  = AvalonSubordinateOut
  { so_readData :: KeepType (KeepReadData (SShared config))
                     (Unsigned (DataWidth (SShared config)))
  , so_readDataValid :: KeepType (KeepReadDataValid (SShared config)) Bool
  , so_endOfPacket :: KeepType (KeepEndOfPacket (SShared config)) Bool
  , so_waitRequest :: KeepType (KeepWaitRequest config) Bool
  , so_readyForData :: KeepType (KeepReadyForData config) Bool
  , so_dataAvailable :: KeepType (KeepDataAvailable config) Bool
  , so_irq :: KeepType (KeepIrq config) Bool
  }
  deriving (Generic, Bundle)

deriving instance (KnownMMSubordinateConfig config)
                   => NFDataX (AvalonSubordinateOut config)
deriving instance (KnownMMSubordinateConfig config)
                   => NFData (AvalonSubordinateOut config)
deriving instance (KnownMMSubordinateConfig config)
                   => ShowX (AvalonSubordinateOut config)
deriving instance (KnownMMSubordinateConfig config)
                   => Show (AvalonSubordinateOut config)
deriving instance (KnownMMSubordinateConfig config)
                   => Eq (AvalonSubordinateOut config)


-- | Data coming into an Avalon MM subordinate port.
-- All fields are optional and can be toggled using the config.
data AvalonSubordinateIn config
  = AvalonSubordinateIn
  { si_writeData :: KeepType (KeepWriteData (SShared config))
                      (Unsigned (DataWidth (SShared config)))
  , si_addr :: KeepType (KeepAddr config) (Unsigned (AddrWidth (SShared config)))
  , si_read :: KeepType (KeepRead (SShared config)) Bool
  , si_write :: KeepType (KeepWrite (SShared config)) Bool
  , si_byteEnable :: KeepType (KeepByteEnable (SShared config))
                       (Unsigned (ByteEnableWidth (SShared config)))
  , si_burstCount :: KeepType (KeepBurstCount (SShared config))
                       (Unsigned (BurstCountWidth (SShared config)))
  , si_writeByteEnable :: KeepType (KeepWriteByteEnable config)
                            (Unsigned (ByteEnableWidth (SShared config)))
  , si_chipSelect :: KeepType (KeepChipSelect config) Bool
  , si_beginTransfer :: KeepType (KeepBeginTransfer config) Bool
  , si_beginBurstTransfer :: KeepType (KeepBeginBurstTransfer config) Bool
  }
  deriving (Generic, Bundle)

deriving instance (KnownMMSubordinateConfig config)
                   => NFDataX (AvalonSubordinateIn config)
deriving instance (KnownMMSubordinateConfig config)
                   => NFData (AvalonSubordinateIn config)
deriving instance (KnownMMSubordinateConfig config)
                   => ShowX (AvalonSubordinateIn config)
deriving instance (KnownMMSubordinateConfig config)
                   => Show (AvalonSubordinateIn config)
deriving instance (KnownMMSubordinateConfig config)
                   => Eq (AvalonSubordinateIn config)


-- | "Important" data pertaining to write transfers.
-- Can be grabbed from 'AvalonManagerOut' or 'AvalonSubordinateIn'.
data AvalonWriteImpt keepAddr config
  = AvalonWriteImpt
  { wi_writeData  :: KeepType (KeepWriteData config)
                       (Unsigned (DataWidth config))
  , wi_addr       :: KeepType keepAddr (Unsigned (AddrWidth config))
  , wi_byteEnable :: KeepType (KeepByteEnable config)
                       (Unsigned (ByteEnableWidth config))
  , wi_burstCount :: KeepType (KeepBurstCount config)
                       (Unsigned (BurstCountWidth config))
  }
  deriving (Generic, Bundle)

deriving instance (KnownMMSharedConfig config,
                   NFDataX (KeepType keepAddr (Unsigned (AddrWidth config))),
                   KeepTypeClass keepAddr)
                   => NFDataX (AvalonWriteImpt keepAddr config)
deriving instance (KnownMMSharedConfig config,
                   NFData (KeepType keepAddr (Unsigned (AddrWidth config))),
                   KeepTypeClass keepAddr)
                   => NFData (AvalonWriteImpt keepAddr config)
deriving instance (KnownMMSharedConfig config,
                   ShowX (KeepType keepAddr (Unsigned (AddrWidth config))),
                   KeepTypeClass keepAddr)
                   => ShowX (AvalonWriteImpt keepAddr config)
deriving instance (KnownMMSharedConfig config,
                   Show (KeepType keepAddr (Unsigned (AddrWidth config))),
                   KeepTypeClass keepAddr)
                   => Show (AvalonWriteImpt keepAddr config)
deriving instance (KnownMMSharedConfig config,
                   Eq (KeepType keepAddr (Unsigned (AddrWidth config))),
                   KeepTypeClass keepAddr)
                   => Eq (AvalonWriteImpt keepAddr config)

-- | "Important" data pertaining to read requests.
-- Can be grabbed from 'AvalonManagerOut' or 'AvalonSubordinateIn'.
data AvalonReadReqImpt keepAddr config
  = AvalonReadReqImpt
  { rri_addr       :: KeepType keepAddr (Unsigned (AddrWidth config))
  , rri_byteEnable :: KeepType (KeepByteEnable config)
                        (Unsigned (ByteEnableWidth config))
  , rri_burstCount :: KeepType (KeepBurstCount config)
                        (Unsigned (BurstCountWidth config))
  }
  deriving (Generic, Bundle)

deriving instance (KnownMMSharedConfig config,
                   NFDataX (KeepType keepAddr (Unsigned (AddrWidth config))),
                   KeepTypeClass keepAddr)
                   => NFDataX (AvalonReadReqImpt keepAddr config)
deriving instance (KnownMMSharedConfig config,
                   NFData (KeepType keepAddr (Unsigned (AddrWidth config))),
                   KeepTypeClass keepAddr)
                   => NFData (AvalonReadReqImpt keepAddr config)
deriving instance (KnownMMSharedConfig config,
                   ShowX (KeepType keepAddr (Unsigned (AddrWidth config))),
                   KeepTypeClass keepAddr)
                   => ShowX (AvalonReadReqImpt keepAddr config)
deriving instance (KnownMMSharedConfig config,
                   Show (KeepType keepAddr (Unsigned (AddrWidth config))),
                   KeepTypeClass keepAddr)
                   => Show (AvalonReadReqImpt keepAddr config)
deriving instance (KnownMMSharedConfig config,
                   Eq (KeepType keepAddr (Unsigned (AddrWidth config))),
                   KeepTypeClass keepAddr)
                   => Eq (AvalonReadReqImpt keepAddr config)

-- | "Important" data pertaining to subordinate-to-master read data.
-- Can be grabbed from 'AvalonManagerIn' or 'AvalonSubordinateOut'.
data AvalonReadImpt config
  = AvalonReadImpt
  { ri_readData    :: KeepType (KeepReadData config)
                        (Unsigned (DataWidth config))
  , ri_endOfPacket :: KeepType (KeepEndOfPacket config) Bool
  }
  deriving (Generic, Bundle)

deriving instance (KnownMMSharedConfig config)
                   => NFDataX (AvalonReadImpt config)
deriving instance (KnownMMSharedConfig config)
                   => NFData (AvalonReadImpt config)
deriving instance (KnownMMSharedConfig config)
                   => ShowX (AvalonReadImpt config)
deriving instance (KnownMMSharedConfig config)
                   => Show (AvalonReadImpt config)
deriving instance (KnownMMSharedConfig config)
                   => Eq (AvalonReadImpt config)


-- | Given an 'AvalonManagerOut', separate out the parts which are not
-- compatible with 'DfConv.DfConv'.
managerOutRemoveNonDf ::
  KnownMMManagerConfig cfg =>
  AvalonManagerOut cfg ->
  ( AvalonManagerOut (RemoveNonDfManager cfg)
  , KeepType (KeepFlush cfg) Bool )
managerOutRemoveNonDf AvalonManagerOut{..}
  = (AvalonManagerOut
  { mo_addr, mo_read, mo_write, mo_byteEnable, mo_burstCount, mo_writeData
  , mo_flush = keepTypeFalse
  }, mo_flush)

-- | Given an 'AvalonManagerOut' which is compatible with 'DfConv.DfConv', add
-- back in the parts which are not compatible with 'DfConv.DfConv'.
managerOutAddNonDf ::
  KnownMMManagerConfig cfg =>
  KeepType (KeepFlush cfg) Bool ->
  AvalonManagerOut (RemoveNonDfManager cfg) ->
  AvalonManagerOut cfg
managerOutAddNonDf flush AvalonManagerOut{..}
  = AvalonManagerOut
  { mo_addr, mo_read, mo_write, mo_byteEnable, mo_burstCount, mo_writeData
  , mo_flush = flush
  }

-- | Given an 'AvalonManagerIn', separate out the parts which are not
-- compatible with 'DfConv.DfConv'.
managerInRemoveNonDf ::
  KnownMMManagerConfig cfg =>
  AvalonManagerIn cfg ->
  ( AvalonManagerIn (RemoveNonDfManager cfg)
  , ( KeepType (KeepIrqList cfg) (Unsigned 32)
    , KeepType (KeepIrqNumber cfg) (Maybe (Unsigned 6)) ) )
managerInRemoveNonDf AvalonManagerIn{..}
  = (AvalonManagerIn
  { mi_readData, mi_waitRequest, mi_readDataValid, mi_endOfPacket
  , mi_irqList = keepTypeFalse
  , mi_irqNumber = keepTypeFalse
  }, (mi_irqList, mi_irqNumber))

-- | Given an 'AvalonManagerIn' which is compatible with 'DfConv.DfConv', add
-- back in the parts which are not compatible with 'DfConv.DfConv'.
managerInAddNonDf ::
  KnownMMManagerConfig cfg =>
  ( KeepType (KeepIrqList cfg) (Unsigned 32)
  , KeepType (KeepIrqNumber cfg) (Maybe (Unsigned 6)) ) ->
  AvalonManagerIn (RemoveNonDfManager cfg) ->
  AvalonManagerIn cfg
managerInAddNonDf (irqList, irqNumber) AvalonManagerIn{..}
  = AvalonManagerIn
  { mi_readData, mi_waitRequest, mi_readDataValid, mi_endOfPacket
  , mi_irqList = irqList
  , mi_irqNumber = irqNumber
  }

-- | Given an 'AvalonSubordinateOut', separate out the parts which are not
-- compatible with 'DfConv.DfConv'.
subordinateOutRemoveNonDf ::
  KnownMMSubordinateConfig cfg =>
  AvalonSubordinateOut cfg ->
  ( AvalonSubordinateOut (RemoveNonDfSubordinate cfg)
  , ( KeepType (KeepReadyForData cfg) Bool
    , KeepType (KeepDataAvailable cfg) Bool
    , KeepType (KeepIrq cfg) Bool ) )
subordinateOutRemoveNonDf AvalonSubordinateOut{..}
  = (AvalonSubordinateOut
  { so_waitRequest, so_readDataValid, so_endOfPacket, so_readData
  , so_readyForData = keepTypeFalse
  , so_dataAvailable = keepTypeFalse
  , so_irq = keepTypeFalse
  }, (so_readyForData, so_dataAvailable, so_irq))

-- | Given an 'AvalonSubordinateOut' which is compatible with 'DfConv.DfConv',
-- add back in the parts which are not compatible with 'DfConv.DfConv'.
subordinateOutAddNonDf ::
  KnownMMSubordinateConfig cfg =>
  ( KeepType (KeepReadyForData cfg) Bool
  , KeepType (KeepDataAvailable cfg) Bool
  , KeepType (KeepIrq cfg) Bool ) ->
  AvalonSubordinateOut (RemoveNonDfSubordinate cfg) ->
  AvalonSubordinateOut cfg
subordinateOutAddNonDf (readyForData, dataAvailable, irq) AvalonSubordinateOut{..}
  = AvalonSubordinateOut
  { so_waitRequest, so_readDataValid, so_endOfPacket, so_readData
  , so_readyForData = readyForData
  , so_dataAvailable = dataAvailable
  , so_irq = irq
  }

-- | Given an 'AvalonSubordinateIn', separate out the parts which are not
-- compatible with 'DfConv.DfConv'.
subordinateInRemoveNonDf ::
  KnownMMSubordinateConfig cfg =>
  AvalonSubordinateIn cfg ->
  ( AvalonSubordinateIn (RemoveNonDfSubordinate cfg)
  , ( KeepType (KeepBeginBurstTransfer cfg) Bool
    , KeepType (KeepBeginTransfer cfg) Bool ) )
subordinateInRemoveNonDf AvalonSubordinateIn{..}
  = (AvalonSubordinateIn
  { si_chipSelect, si_addr, si_read, si_write, si_byteEnable
  , si_writeByteEnable, si_burstCount, si_writeData
  , si_beginBurstTransfer = keepTypeFalse
  , si_beginTransfer = keepTypeFalse
  }, (si_beginBurstTransfer, si_beginTransfer))

-- | Given an 'AvalonSubordinateIn' which is compatible with 'DfConv.DfConv',
-- add back in the parts which are not compatible with 'DfConv.DfConv'.
subordinateInAddNonDf ::
  KnownMMSubordinateConfig cfg =>
  ( KeepType (KeepBeginBurstTransfer cfg) Bool
  , KeepType (KeepBeginTransfer cfg) Bool ) ->
  AvalonSubordinateIn (RemoveNonDfSubordinate cfg) ->
  AvalonSubordinateIn cfg
subordinateInAddNonDf (beginBurstTransfer, beginTransfer) AvalonSubordinateIn{..}
  = AvalonSubordinateIn
  { si_chipSelect, si_addr, si_read, si_write, si_byteEnable
  , si_writeByteEnable, si_burstCount, si_writeData
  , si_beginBurstTransfer = beginBurstTransfer
  , si_beginTransfer = beginTransfer
  }

-- | Convert a boolean value to an @AvalonSubordinateOut@ structure.
-- The structure gives no read data, no IRQ, etc. Fields relating to
-- "acknowledging" a write, or a read request, are controlled by the bool input.
boolToMMSubordinateAck ::
  (KnownMMSubordinateConfig config) => Bool -> AvalonSubordinateOut config
boolToMMSubordinateAck ack
  = AvalonSubordinateOut
    { so_waitRequest   = toKeepType (not ack)
    , so_readDataValid = toKeepType False
    , so_readyForData  = toKeepType ack
    , so_dataAvailable = toKeepType False
    , so_endOfPacket   = toKeepType False
    , so_irq           = toKeepType False
    , so_readData      = errorX "No readData for boolToAck"
    }

-- | Convert a boolean value to an @AvalonManagerIn@ structure.
-- The structure gives no read data, no IRQ, etc. The @waitRequest@ field is
-- controlled by the (negated) boolean input.
boolToMMManagerAck ::
  (KnownMMManagerConfig config) => Bool -> AvalonManagerIn config
boolToMMManagerAck ack
  = AvalonManagerIn
  { mi_waitRequest   = not ack
  , mi_readDataValid = toKeepType False
  , mi_endOfPacket   = toKeepType False
  , mi_irqList       = toKeepType 0
  , mi_irqNumber     = toKeepType Nothing
  , mi_readData      = errorX "No readData for boolToAck"
  }

-- | Convert an 'AvalonReadImpt' into an 'AvalonSubordinateOut' containing that
-- read data.
mmReadImptToSubordinateOut ::
  (KnownMMSubordinateConfig config, config ~ RemoveNonDfSubordinate config) =>
  AvalonReadImpt (SShared config) -> AvalonSubordinateOut config
mmReadImptToSubordinateOut dat
  = AvalonSubordinateOut
    { so_waitRequest   = toKeepType False
    , so_readDataValid = toKeepType True
    , so_readyForData  = keepTypeFalse
    , so_dataAvailable = keepTypeFalse
    , so_endOfPacket   = ri_endOfPacket dat
    , so_irq           = keepTypeFalse
    , so_readData      = ri_readData dat
    }

-- | Grab the important read information from an 'AvalonSubordinateOut', putting
-- it into an 'AvalonReadImpt'. Only works if fixed wait time is 0.
mmSubordinateOutToReadImpt ::
  (KnownMMSubordinateConfig config, config ~ RemoveNonDfSubordinate config) =>
  AvalonSubordinateOut config -> Maybe (AvalonReadImpt (SShared config))
mmSubordinateOutToReadImpt (AvalonSubordinateOut{..})
  = if cond then Just AvalonReadImpt
  { ri_endOfPacket = so_endOfPacket
  , ri_readData    = so_readData
  } else Nothing
  where
  cond =  fromKeepTypeDef True so_readDataValid
       && not (fromKeepTypeDef False so_waitRequest)

-- | Convert an 'AvalonReadImpt' into an 'AvalonManagerIn' containing that
-- read data.
mmReadImptToManagerIn ::
  (KnownMMManagerConfig config, config ~ RemoveNonDfManager config) =>
  AvalonReadImpt (MShared config) -> AvalonManagerIn config
mmReadImptToManagerIn dat
  = AvalonManagerIn
  { mi_waitRequest   = False
  , mi_readDataValid = toKeepType True
  , mi_endOfPacket   = ri_endOfPacket dat
  , mi_irqList       = keepTypeFalse
  , mi_irqNumber     = keepTypeFalse
  , mi_readData      = ri_readData dat
  }

-- | Grab the important read information from an 'AvalonManagerIn', putting it
-- into an 'AvalonReadImpt'. Only works if fixed wait time is 0.
mmManagerInToReadImpt ::
  (KnownMMManagerConfig config, config ~ RemoveNonDfManager config) =>
  AvalonManagerIn config -> Maybe (AvalonReadImpt (MShared config))
mmManagerInToReadImpt (AvalonManagerIn{..})
  = if cond then Just AvalonReadImpt
  { ri_endOfPacket = mi_endOfPacket
  , ri_readData    = mi_readData
  } else Nothing
  where
  cond = fromKeepTypeDef True mi_readDataValid

-- | Convert an 'AvalonWriteImpt' into an 'AvalonSubordinateIn' containing
-- that write data.
mmWriteImptToSubordinateIn ::
  (KnownMMSubordinateConfig config, config ~ RemoveNonDfSubordinate config) =>
  AvalonWriteImpt (KeepAddr config) (SShared config) ->
  AvalonSubordinateIn config
mmWriteImptToSubordinateIn (AvalonWriteImpt{..})
  = AvalonSubordinateIn
  { si_chipSelect         = toKeepType True
  , si_addr               = wi_addr
  , si_read               = toKeepType False
  , si_write              = toKeepType True
  , si_byteEnable         = wi_byteEnable
  , si_writeByteEnable    = convKeepType
                            (bitCoerce $ repeat True)
                            wi_byteEnable
  , si_beginTransfer      = keepTypeFalse
  , si_burstCount         = wi_burstCount
  , si_beginBurstTransfer = keepTypeFalse
  , si_writeData          = wi_writeData
  }

-- | Grab the important write information from an 'AvalonSubordinateIn',
-- putting it into an 'AvalonWriteImpt'.
mmSubordinateInToWriteImpt ::
  (KnownMMSubordinateConfig config, config ~ RemoveNonDfSubordinate config) =>
  AvalonSubordinateIn config ->
  Maybe (AvalonWriteImpt (KeepAddr config) (SShared config))
mmSubordinateInToWriteImpt (AvalonSubordinateIn{..})
  = if cond then Just AvalonWriteImpt
  { wi_addr               = si_addr
  , wi_byteEnable         = si_byteEnable
  , wi_burstCount         = si_burstCount
  , wi_writeData          = si_writeData
  } else Nothing
  where
  cond =  fromKeepTypeDef True si_chipSelect
       && fromKeepTypeDef True si_write
       && not (fromKeepTypeDef False si_read)

-- | Convert an 'AvalonWriteImpt' into an 'AvalonManagerOut' containing that
-- write data.
mmWriteImptToManagerOut ::
  (KnownMMManagerConfig config, config ~ RemoveNonDfManager config) =>
  AvalonWriteImpt 'True (MShared config) -> AvalonManagerOut config
mmWriteImptToManagerOut (AvalonWriteImpt{..})
  = AvalonManagerOut
  { mo_addr        = fromKeepTypeTrue wi_addr
  , mo_read        = toKeepType False
  , mo_write       = toKeepType True
  , mo_byteEnable  = wi_byteEnable
  , mo_burstCount  = wi_burstCount
  , mo_flush       = keepTypeFalse
  , mo_writeData   = wi_writeData
  }

-- | Grab the important write information from an 'AvalonManagerOut', putting
-- it into an 'AvalonWriteImpt'.
mmManagerOutToWriteImpt ::
  (KnownMMManagerConfig config, config ~ RemoveNonDfManager config) =>
  AvalonManagerOut config -> Maybe (AvalonWriteImpt 'True (MShared config))
mmManagerOutToWriteImpt (AvalonManagerOut{..})
  = if cond then Just AvalonWriteImpt
  { wi_addr       = toKeepType mo_addr
  , wi_byteEnable = mo_byteEnable
  , wi_burstCount = mo_burstCount
  , wi_writeData  = mo_writeData
  } else Nothing
  where
  cond =  fromKeepTypeDef True mo_write
       && not (fromKeepTypeDef False mo_read)

-- | Convert an 'AvalonReadReqImpt' into an 'AvalonSubordinateIn' containing
-- that read request data.
mmReadReqImptToSubordinateIn ::
  (KnownMMSubordinateConfig config, config ~ RemoveNonDfSubordinate config) =>
  AvalonReadReqImpt (KeepAddr config) (SShared config) ->
  AvalonSubordinateIn config
mmReadReqImptToSubordinateIn (AvalonReadReqImpt{..})
  = AvalonSubordinateIn
  { si_chipSelect         = toKeepType True
  , si_addr               = rri_addr
  , si_read               = toKeepType True
  , si_write              = toKeepType False
  , si_byteEnable         = rri_byteEnable
  , si_writeByteEnable    = toKeepType 0
  , si_beginTransfer      = keepTypeFalse
  , si_burstCount         = rri_burstCount
  , si_beginBurstTransfer = keepTypeFalse
  , si_writeData          = errorX "No writeData for read req"
  }

-- | Grab the important read request information from an 'AvalonSubordinateIn',
-- putting it into an 'AvalonReadReqImpt'.
mmSubordinateInToReadReqImpt ::
  (KnownMMSubordinateConfig config, config ~ RemoveNonDfSubordinate config) =>
  AvalonSubordinateIn config ->
  Maybe (AvalonReadReqImpt (KeepAddr config) (SShared config))
mmSubordinateInToReadReqImpt (AvalonSubordinateIn{..})
  = if cond then Just AvalonReadReqImpt
  { rri_addr               = si_addr
  , rri_byteEnable         = si_byteEnable
  , rri_burstCount         = si_burstCount
  } else Nothing
  where
  cond =  fromKeepTypeDef True si_chipSelect
       && fromKeepTypeDef True si_read
       && not (fromKeepTypeDef False si_write)

-- | Convert an 'AvalonReadReqImpt' into an 'AvalonManagerOut' containing that
-- read request data.
mmReadReqImptToManagerOut ::
  (KnownMMManagerConfig config, config ~ RemoveNonDfManager config) =>
  AvalonReadReqImpt 'True (MShared config) -> AvalonManagerOut config
mmReadReqImptToManagerOut (AvalonReadReqImpt{..})
  = AvalonManagerOut
  { mo_addr        = fromKeepTypeTrue rri_addr
  , mo_read        = toKeepType True
  , mo_write       = toKeepType False
  , mo_byteEnable  = rri_byteEnable
  , mo_burstCount  = rri_burstCount
  , mo_flush       = keepTypeFalse
  , mo_writeData   = errorX "No writeData for read req"
  }

-- | Grab the important read request information from an 'AvalonManagerOut',
-- putting it into an 'AvalonReadReqImpt'.
mmManagerOutToReadReqImpt ::
  (KnownMMManagerConfig config, config ~ RemoveNonDfManager config) =>
  AvalonManagerOut config -> Maybe (AvalonReadReqImpt 'True (MShared config))
mmManagerOutToReadReqImpt (AvalonManagerOut{..})
  = if cond then Just AvalonReadReqImpt
  { rri_addr       = toKeepType mo_addr
  , rri_byteEnable = mo_byteEnable
  , rri_burstCount = mo_burstCount
  } else Nothing
  where
  cond =  fromKeepTypeDef True mo_read
       && not (fromKeepTypeDef False mo_write)

-- | An 'AvalonSubordinateIn' containing no write data, and indicating that no
-- transmission is currently occurring.
mmSubordinateInNoData :: (KnownMMSubordinateConfig config) =>
  AvalonSubordinateIn config
mmSubordinateInNoData
  = AvalonSubordinateIn
  { si_chipSelect         = toKeepType False
  , si_addr               = toKeepType 0
  , si_read               = toKeepType False
  , si_write              = toKeepType False
  , si_byteEnable         = toKeepType 0
  , si_writeByteEnable    = toKeepType 0
  , si_beginTransfer      = toKeepType False
  , si_burstCount         = toKeepType 0
  , si_beginBurstTransfer = toKeepType False
  , si_writeData          = errorX "No writeData for noData"
  }

-- | An 'AvalonManagerOut' containing no write data, and indicating that no
-- transmission is currently occurring.
mmManagerOutNoData :: (KnownMMManagerConfig config) => AvalonManagerOut config
mmManagerOutNoData
  = AvalonManagerOut
  { mo_addr        = 0
  , mo_read        = toKeepType False
  , mo_write       = toKeepType False
  , mo_byteEnable  = toKeepType 0
  , mo_burstCount  = toKeepType 0
  , mo_flush       = toKeepType False
  , mo_writeData   = errorX "No writeData for noData"
  }

-- | Grab the "acknowledgement" value from an 'AvalonSubordinateOut'.
-- Reasonable defaults are provided for optional fields.
mmSubordinateOutToBool ::
  (KnownMMSubordinateConfig config) =>
  AvalonSubordinateOut config -> Bool
mmSubordinateOutToBool so
  =  fromKeepTypeDef True (so_readyForData so)
  && not (fromKeepTypeDef False (so_waitRequest so))

-- | Grab the "acknowledgement" value from an 'AvalonManagerIn'.
-- Reasonable defaults are provided for optional fields.
mmManagerInToBool ::
  (KnownMMManagerConfig config) => AvalonManagerIn config -> Bool
mmManagerInToBool = not . mi_waitRequest

-- | Datatype for the manager end of the Avalon memory-mapped protocol.
data AvalonMMManager (dom :: Domain) (config :: AvalonMMManagerConfig)
  = AvalonMMManager

-- | Datatype for the subordinate end of the Avalon memory-mapped protocol.
data AvalonMMSubordinate
       (dom :: Domain)
       (fixedWaitTime :: Nat)
       (config :: AvalonMMSubordinateConfig)
       = AvalonMMSubordinate

instance Protocol (AvalonMMManager dom config) where
  type Fwd (AvalonMMManager dom config) = Signal dom (AvalonManagerOut config)
  type Bwd (AvalonMMManager dom config) = Signal dom (AvalonManagerIn config)

instance Protocol (AvalonMMSubordinate dom fixedWaitTime config) where
  type Fwd (AvalonMMSubordinate dom fixedWaitTime config)
         = Signal dom (AvalonSubordinateIn config)
  type Bwd (AvalonMMSubordinate dom fixedWaitTime config)
         = Signal dom (AvalonSubordinateOut config)

instance (KnownMMSubordinateConfig config, KeepWaitRequest config ~ 'True) =>
  Backpressure (AvalonMMSubordinate dom 0 config) where

  boolsToBwd _ = C.fromList_lazy . fmap boolToMMSubordinateAck

instance (KnownMMManagerConfig config) =>
  Backpressure (AvalonMMManager dom config) where

  boolsToBwd _ = C.fromList_lazy . fmap boolToMMManagerAck

-- 'toDfLike' and 'fromDfLike' here are very similar to their implementations
-- in @instance DfConv.DfConv (AvalonMMManager ...)@. 'toDfCircuit' forces
-- burstCount to be 1, will overwrite whatever value you specify in your
-- WriteImpt and ReadReqImpt. In the case of write, this is because the user
-- could otherwise send in a write with burst size of 2, but then send a read
-- request along the same channel, which would either have to get dropped, or
-- the write burst would have to be left unfinished. In the case of read
-- request, this is because read data only comes in for one clock cycle, while
-- a Df acknowledge can take a while to come in, so we need to store incoming
-- read data; if we were allowed to have arbitrarily large burst sizes, we
-- would need to store incoming read data in an arbitrarily-large buffer.
instance
  (KnownMMSubordinateConfig config, config ~ RemoveNonDfSubordinate config) =>
  DfConv.DfConv   (AvalonMMSubordinate dom 0 config) where

  type Dom        (AvalonMMSubordinate dom 0 config) = dom
  type BwdPayload (AvalonMMSubordinate dom 0 config)
         = AvalonReadImpt (SShared config)
  type FwdPayload (AvalonMMSubordinate dom 0 config)
         = Either
             (AvalonReadReqImpt (KeepAddr config) (SShared config))
             (AvalonWriteImpt (KeepAddr config) (SShared config))

  toDfCircuit proxy = DfConv.toDfCircuitHelper proxy s0 blankOtp stateFn where
    s0 = (Nothing, False)
    -- Nothing: readDatStored:
    --   reads only get sent for one clock cycle, so we have to store it
    --   until it's acked
    -- False: readReqAcked:
    --   when our read request is acknowledged, we should stop sending the
    --   read request forwards
    blankOtp = mmSubordinateInNoData
    stateFn so dfAck dfDat = do
      (readDatStored, readReqAcked) <- get
      let readDatIn = mmSubordinateOutToReadImpt so
      let miBool = mmSubordinateOutToBool so
      let (readDatStored', readReqAcked', si, dfAckOut)
            = case (dfDat, readDatStored) of
                (Just (Right wi), _) ->
                  ( readDatStored
                  , False
                  , mmWriteImptToSubordinateIn
                      (wi { wi_burstCount = toKeepType 1 })
                  , miBool )
                (_, Just _) ->
                  ( readDatStored
                  , False
                  , mmSubordinateInNoData
                  , miBool )
                (Just (Left ri), Nothing) ->
                  ( readDatIn
                  , miBool
                  , if readReqAcked
                      then mmSubordinateInNoData
                      else mmReadReqImptToSubordinateIn
                        (ri { rri_burstCount = toKeepType 1 })
                  , miBool )
                (Nothing, Nothing) ->
                  ( Nothing
                  , False
                  , mmSubordinateInNoData
                  , False )
      put
        ( if ( Maybe.isNothing readDatStored' -- to avoid looking at dfAck when not needed
            || dfAck) then Nothing else readDatStored'
        , readReqAcked')
      pure (si, readDatStored', dfAckOut)

  fromDfCircuit proxy = DfConv.fromDfCircuitHelper proxy s0 blankOtp stateFn where
    s0 = False -- dfAckSt -- read request might be acked before read is sent back
    blankOtp = boolToMMSubordinateAck False
    stateFn si dfAck dfDat = do
      dfAckSt <- get
      let writeImpt = mmSubordinateInToWriteImpt si
      let readReqImpt
            = if Maybe.isNothing writeImpt
              then mmSubordinateInToReadReqImpt si
              else Nothing
      let sendingReadDat
            = if (Maybe.isJust readReqImpt) && dfAckSt
              then dfDat else Nothing
      let dfAckSt' = Maybe.isJust readReqImpt && (dfAckSt || dfAck)
      let so = case (writeImpt, sendingReadDat) of
                 (Just _, _) -> boolToMMSubordinateAck dfAck
                 (_, Just rdat) -> mmReadImptToSubordinateOut rdat
                 _ -> boolToMMSubordinateAck False
      let dfDatOut = case (writeImpt, readReqImpt, dfAckSt) of
                       (Just wi, _, _) -> Just (Right wi)
                       (_, Just ri, False) -> Just (Left ri)
                       _ -> Nothing
      let dfAckOut = Maybe.isJust sendingReadDat
      put dfAckSt'
      pure (so, dfDatOut, dfAckOut)

-- See comments for @instance DfConv.DfConv (AvalonMMSubordinate ...)@, as the
-- instance is very similar and the comments there apply here as well.
instance (KnownMMManagerConfig config, config ~ RemoveNonDfManager config) =>
  DfConv.DfConv   (AvalonMMManager dom config) where
  type Dom        (AvalonMMManager dom config) = dom
  type BwdPayload (AvalonMMManager dom config) = AvalonReadImpt (MShared config)
  type FwdPayload (AvalonMMManager dom config)
         = Either
           (AvalonReadReqImpt 'True (MShared config))
           (AvalonWriteImpt 'True (MShared config))

  toDfCircuit proxy = DfConv.toDfCircuitHelper proxy s0 blankOtp stateFn where
    s0 = (Nothing, False)
    -- Nothing: readDatStored:
    --   reads only get sent for one clock cycle, so we have to store it
    --   until it's acked
    -- False: readReqAcked:
    --   when our read request is acknowledged, we should stop sending the
    --   read request forwards
    blankOtp = mmManagerOutNoData
    stateFn mi dfAck dfDat = do
      (readDatStored, readReqAcked) <- get
      let readDatIn = mmManagerInToReadImpt mi
      let miBool = mmManagerInToBool mi
      let (readDatStored', readReqAcked', mo, dfAckOut)
            = case (dfDat, readDatStored) of
                (Just (Right wi), _) ->
                  ( readDatStored
                  , False
                  , mmWriteImptToManagerOut
                      (wi { wi_burstCount = toKeepType 1 })
                  , miBool )
                (_, Just _) ->
                  ( readDatStored
                  , False
                  , mmManagerOutNoData
                  , miBool )
                (Just (Left ri), Nothing) ->
                  ( readDatIn
                  , miBool
                  , if readReqAcked
                      then mmManagerOutNoData
                      else mmReadReqImptToManagerOut
                             (ri { rri_burstCount = toKeepType 1 })
                  , miBool )
                (Nothing, Nothing) ->
                  ( Nothing
                  , False
                  , mmManagerOutNoData
                  , False )
      put
        ( if ( Maybe.isNothing readDatStored' -- to avoid looking at dfAck when not needed
            || dfAck) then Nothing else readDatStored'
        , readReqAcked')
      pure (mo, readDatStored', dfAckOut)

  fromDfCircuit proxy = DfConv.fromDfCircuitHelper proxy s0 blankOtp stateFn where
    s0 = False -- dfAckSt -- read request might be acked before read is sent back
    blankOtp = boolToMMManagerAck False
    stateFn mo dfAck dfDat = do
      dfAckSt <- get
      let writeImpt = mmManagerOutToWriteImpt mo
      let readReqImpt
            = if Maybe.isNothing writeImpt
              then mmManagerOutToReadReqImpt mo
              else Nothing
      let sendingReadDat
            = if (Maybe.isJust readReqImpt) && dfAckSt
              then dfDat else Nothing
      let dfAckSt' = Maybe.isJust readReqImpt && (dfAckSt || dfAck)
      let mi = case (writeImpt, sendingReadDat) of
                 (Just _, _) -> boolToMMManagerAck dfAck
                 (_, Just rdat) -> mmReadImptToManagerIn rdat
                 _ -> boolToMMManagerAck False
      let dfDatOut = case (writeImpt, readReqImpt, dfAckSt) of
                       (Just wi, _, _) -> Just (Right wi)
                       (_, Just ri, False) -> Just (Left ri)
                       _ -> Nothing
      let dfAckOut = Maybe.isJust sendingReadDat
      put dfAckSt'
      pure (mi, dfDatOut, dfAckOut)

instance
  ( KnownMMManagerConfig config
  , KnownDomain dom
  , config ~ RemoveNonDfManager config) =>
  Simulate (AvalonMMManager dom config) where

  type SimulateFwdType (AvalonMMManager dom config) = [AvalonManagerOut config]
  type SimulateBwdType (AvalonMMManager dom config) = [AvalonManagerIn config]
  type SimulateChannels (AvalonMMManager dom config) = 1

  simToSigFwd _ = fromList_lazy
  simToSigBwd _ = fromList_lazy
  sigToSimFwd _ = sample_lazy
  sigToSimBwd _ = sample_lazy

  stallC conf (head -> (stallAck, stalls))
    = withClockResetEnable clockGen resetGen enableGen
    $ DfConv.stall Proxy Proxy conf stallAck stalls

instance
  ( KnownMMSubordinateConfig config
  , KnownDomain dom
  , config ~ RemoveNonDfSubordinate config) =>
  Simulate (AvalonMMSubordinate dom 0 config) where

  type SimulateFwdType (AvalonMMSubordinate dom 0 config)
         = [AvalonSubordinateIn config]
  type SimulateBwdType (AvalonMMSubordinate dom 0 config)
         = [AvalonSubordinateOut config]
  type SimulateChannels (AvalonMMSubordinate dom 0 config) = 1

  simToSigFwd _ = fromList_lazy
  simToSigBwd _ = fromList_lazy
  sigToSimFwd _ = sample_lazy
  sigToSimBwd _ = sample_lazy

  stallC conf (head -> (stallAck, stalls))
    = withClockResetEnable clockGen resetGen enableGen
    $ DfConv.stall Proxy Proxy conf stallAck stalls

-- NOTE: Unfortunately, we can't write a 'Drivable' instance (and, by extension,
-- a 'Test' instance) for 'AvalonMMManager' or 'AvalonMMSubordinate'.  This is
-- because they have both write data sent one way and read data sent the other.
-- By writing a 'Drivable' instance (meant for protocols with unidirectional
-- data), we would have to ignore one or the other.
--
-- Tests can still be made for Avalon MM circuits, using 'DfConv.dfConvTestBench'.
-- See 'Tests.Protocols.AvalonMemMap' for examples.
