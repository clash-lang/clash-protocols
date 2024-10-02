{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Types and instance declarations for the Avalon memory mapped protocol
(http://www1.cs.columbia.edu/~sedwards/classes/2009/4840/mnl_avalon_spec.pdf).
Non-required fields can be easily toggled by the user. The @data@ and
@outputenable@ fields are not supported since we would need bidirectional data
ports. The @resetrequest@ field is also not supported since this does not get
transferred around, but rather gets send "outwards" to whoever is controlling
the reset signal of the circuit.
-}
module Protocols.Avalon.MemMap (
  -- * Configuration types
  AvalonMmSharedConfig (..),
  AvalonMmSubordinateConfig (..),
  AvalonMmManagerConfig (..),

  -- * Grab members from AvalonMmSharedConfig at the type level
  DataWidth,
  KeepReadData,
  KeepWriteData,
  AddrWidth,
  KeepRead,
  KeepWrite,
  ByteEnableWidth,
  KeepByteEnable,
  BurstCountWidth,
  KeepBurstCount,
  KeepReadDataValid,
  KeepEndOfPacket,

  -- * Grab members from AvalonMmSubordinateConfig at the type level
  KeepAddr,
  KeepWriteByteEnable,
  KeepChipSelect,
  KeepBeginTransfer,
  KeepWaitRequest,
  KeepBeginBurstTransfer,
  KeepReadyForData,
  KeepDataAvailable,
  KeepIrq,
  SShared,

  -- * Grab members from AvalonMmManagerConfig at the type level
  KeepFlush,
  KeepIrqList,
  KeepIrqNumber,
  MShared,

  -- * Remove DfConv-incompatible fields from configs
  RemoveNonDfSubordinate,
  RemoveNonDfManager,

  -- * Constraints on configs
  KnownSharedConfig,
  KnownSubordinateConfig,
  KnownManagerConfig,

  -- * Avalon MM signals
  AvalonManagerOut (..),
  AvalonManagerIn (..),
  AvalonSubordinateOut (..),
  AvalonSubordinateIn (..),

  -- * Important parts of signals, used as payload in DfConv
  AvalonWriteImpt (..),
  AvalonReadReqImpt (..),
  AvalonReadImpt (..),

  -- * Helper functions
  managerOutAddNonDf,
  managerOutRemoveNonDf,
  managerInAddNonDf,
  managerInRemoveNonDf,
  subordinateOutAddNonDf,
  subordinateOutRemoveNonDf,
  subordinateInAddNonDf,
  subordinateInRemoveNonDf,
  forceResetSanity,

  -- * Protocols
  AvalonMmManager (..),
  AvalonMmSubordinate (..),
) where

-- base
import Prelude ()

import Control.DeepSeq (NFData)
import Control.Monad.State (get, put)
import qualified Data.Maybe as Maybe
import Data.Proxy

-- clash-prelude
import Clash.Prelude hiding (concat, length, take)
import qualified Clash.Prelude as C

-- me

import qualified Protocols.DfConv as DfConv
import Protocols.Idle
import Protocols.Internal

{- | Config needed for both manager and subordinate interfaces.
@Bool@ values represent whether to keep a boolean field or not.
@Nat@ values represent the width of a variable-sized numeric field.
-}
data AvalonMmSharedConfig = AvalonMmSharedConfig
  { dataWidth :: Nat
  , keepReadData :: Bool
  , keepWriteData :: Bool
  , addrWidth :: Nat
  , keepRead :: Bool
  , keepWrite :: Bool
  , byteEnableWidth :: Nat
  , keepByteEnable :: Bool
  , burstCountWidth :: Nat
  , keepBurstCount :: Bool
  , keepReadDataValid :: Bool
  , keepEndOfPacket :: Bool
  }

{- | Config specific to Avalon MM subordinate interfaces.
@Bool@ values represent whether to keep a boolean field or not.
@Nat@ values represent the width of a variable-sized numeric field.
An @AvalonMmSharedConfig@ is also included for the rest of the fields.
-}
data AvalonMmSubordinateConfig = AvalonMmSubordinateConfig
  { keepAddr :: Bool
  , keepWriteByteEnable :: Bool
  , keepChipSelect :: Bool
  , keepBeginTransfer :: Bool
  , keepWaitRequest :: Bool
  , keepBeginBurstTransfer :: Bool
  , keepReadyForData :: Bool
  , keepDataAvailable :: Bool
  , keepIrq :: Bool
  , sShared :: AvalonMmSharedConfig
  }

{- | Config specific to Avalon MM manager interfaces.
@Bool@ values represent whether to keep a boolean field or not.
@Nat@ values represent the width of a variable-sized numeric field.
An @AvalonMmSharedConfig@ is also included for the rest of the fields.
-}
data AvalonMmManagerConfig = AvalonMmManagerConfig
  { keepFlush :: Bool
  , keepIrqList :: Bool
  , keepIrqNumber :: Bool
  , mShared :: AvalonMmSharedConfig
  }

-- Grab record fields at the type level:

-- | Grab 'dataWidth' from 'AvalonMmSharedConfig' at the type level
type family DataWidth (c :: AvalonMmSharedConfig) where
  DataWidth ('AvalonMmSharedConfig a _ _ _ _ _ _ _ _ _ _ _) = a

-- | Grab 'keepReadData' from 'AvalonMmSharedConfig' at the type level
type family KeepReadData (c :: AvalonMmSharedConfig) where
  KeepReadData ('AvalonMmSharedConfig _ a _ _ _ _ _ _ _ _ _ _) = a

-- | Grab 'keepWriteData' from 'AvalonMmSharedConfig' at the type level
type family KeepWriteData (c :: AvalonMmSharedConfig) where
  KeepWriteData ('AvalonMmSharedConfig _ _ a _ _ _ _ _ _ _ _ _) = a

-- | Grab 'addrWidth' from 'AvalonMmSharedConfig' at the type level
type family AddrWidth (c :: AvalonMmSharedConfig) where
  AddrWidth ('AvalonMmSharedConfig _ _ _ a _ _ _ _ _ _ _ _) = a

-- | Grab 'keepRead' from 'AvalonMmSharedConfig' at the type level
type family KeepRead (c :: AvalonMmSharedConfig) where
  KeepRead ('AvalonMmSharedConfig _ _ _ _ a _ _ _ _ _ _ _) = a

-- | Grab 'keepWrite' from 'AvalonMmSharedConfig' at the type level
type family KeepWrite (c :: AvalonMmSharedConfig) where
  KeepWrite ('AvalonMmSharedConfig _ _ _ _ _ a _ _ _ _ _ _) = a

-- | Grab 'byteEnableWidth' from 'AvalonMmSharedConfig' at the type level
type family ByteEnableWidth (c :: AvalonMmSharedConfig) where
  ByteEnableWidth ('AvalonMmSharedConfig _ _ _ _ _ _ a _ _ _ _ _) = a

-- | Grab 'keepByteEnable' from 'AvalonMmSharedConfig' at the type level
type family KeepByteEnable (c :: AvalonMmSharedConfig) where
  KeepByteEnable ('AvalonMmSharedConfig _ _ _ _ _ _ _ a _ _ _ _) = a

-- | Grab 'burstCountWidth' from 'AvalonMmSharedConfig' at the type level
type family BurstCountWidth (c :: AvalonMmSharedConfig) where
  BurstCountWidth ('AvalonMmSharedConfig _ _ _ _ _ _ _ _ a _ _ _) = a

-- | Grab 'keepBurstCount' from 'AvalonMmSharedConfig' at the type level
type family KeepBurstCount (c :: AvalonMmSharedConfig) where
  KeepBurstCount ('AvalonMmSharedConfig _ _ _ _ _ _ _ _ _ a _ _) = a

-- | Grab 'keepReadDataValid' from 'AvalonMmSharedConfig' at the type level
type family KeepReadDataValid (c :: AvalonMmSharedConfig) where
  KeepReadDataValid ('AvalonMmSharedConfig _ _ _ _ _ _ _ _ _ _ a _) = a

-- | Grab 'keepEndOfPacket' from 'AvalonMmSharedConfig' at the type level
type family KeepEndOfPacket (c :: AvalonMmSharedConfig) where
  KeepEndOfPacket ('AvalonMmSharedConfig _ _ _ _ _ _ _ _ _ _ _ a) = a

-- | Grab 'keepAddr' from 'AvalonMmSubordinateConfig' at the type level
type family KeepAddr (c :: AvalonMmSubordinateConfig) where
  KeepAddr ('AvalonMmSubordinateConfig a _ _ _ _ _ _ _ _ _) = a

-- | Grab 'keepWriteByteEnable' from 'AvalonMmSubordinateConfig' at the type level
type family KeepWriteByteEnable (c :: AvalonMmSubordinateConfig) where
  KeepWriteByteEnable ('AvalonMmSubordinateConfig _ a _ _ _ _ _ _ _ _) = a

-- | Grab 'keepChipSelect' from 'AvalonMmSubordinateConfig' at the type level
type family KeepChipSelect (c :: AvalonMmSubordinateConfig) where
  KeepChipSelect ('AvalonMmSubordinateConfig _ _ a _ _ _ _ _ _ _) = a

-- | Grab 'keepBeginTransfer' from 'AvalonMmSubordinateConfig' at the type level
type family KeepBeginTransfer (c :: AvalonMmSubordinateConfig) where
  KeepBeginTransfer ('AvalonMmSubordinateConfig _ _ _ a _ _ _ _ _ _) = a

-- | Grab 'keepWaitRequest' from 'AvalonMmSubordinateConfig' at the type level
type family KeepWaitRequest (c :: AvalonMmSubordinateConfig) where
  KeepWaitRequest ('AvalonMmSubordinateConfig _ _ _ _ a _ _ _ _ _) = a

-- | Grab 'keepBeginBurstTransfer' from 'AvalonMmSubordinateConfig' at the type level
type family KeepBeginBurstTransfer (c :: AvalonMmSubordinateConfig) where
  KeepBeginBurstTransfer ('AvalonMmSubordinateConfig _ _ _ _ _ a _ _ _ _) = a

-- | Grab 'keepReadyForData' from 'AvalonMmSubordinateConfig' at the type level
type family KeepReadyForData (c :: AvalonMmSubordinateConfig) where
  KeepReadyForData ('AvalonMmSubordinateConfig _ _ _ _ _ _ a _ _ _) = a

-- | Grab 'keepDataAvailable' from 'AvalonMmSubordinateConfig' at the type level
type family KeepDataAvailable (c :: AvalonMmSubordinateConfig) where
  KeepDataAvailable ('AvalonMmSubordinateConfig _ _ _ _ _ _ _ a _ _) = a

-- | Grab 'keepIrq' from 'AvalonMmSubordinateConfig' at the type level
type family KeepIrq (c :: AvalonMmSubordinateConfig) where
  KeepIrq ('AvalonMmSubordinateConfig _ _ _ _ _ _ _ _ a _) = a

-- | Grab 'sShared' from 'AvalonMmSubordinateConfig' at the type level
type family SShared (c :: AvalonMmSubordinateConfig) where
  SShared ('AvalonMmSubordinateConfig _ _ _ _ _ _ _ _ _ a) = a

-- | Grab 'keepFlush' from 'AvalonMmManagerConfig' at the type level
type family KeepFlush (c :: AvalonMmManagerConfig) where
  KeepFlush ('AvalonMmManagerConfig a _ _ _) = a

-- | Grab 'keepIrqList' from 'AvalonMmManagerConfig' at the type level
type family KeepIrqList (c :: AvalonMmManagerConfig) where
  KeepIrqList ('AvalonMmManagerConfig _ a _ _) = a

-- | Grab 'keepIrqNumber' from 'AvalonMmManagerConfig' at the type level
type family KeepIrqNumber (c :: AvalonMmManagerConfig) where
  KeepIrqNumber ('AvalonMmManagerConfig _ _ a _) = a

-- | Grab 'mShared' from 'AvalonMmManagerConfig' at the type level
type family MShared (c :: AvalonMmManagerConfig) where
  MShared ('AvalonMmManagerConfig _ _ _ a) = a

{- | Disable fields of 'AvalonMmSubordinateConfig' that are not allowed in the
'DfConv.DfConv' instance
-}
type family RemoveNonDfSubordinate (cfg :: AvalonMmSubordinateConfig) where
  RemoveNonDfSubordinate cfg =
    'AvalonMmSubordinateConfig
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

{- | Disable fields of 'AvalonMmManagerConfig' that are not allowed in the
'DfConv.DfConv' instance
-}
type family RemoveNonDfManager (cfg :: AvalonMmManagerConfig) where
  RemoveNonDfManager cfg =
    'AvalonMmManagerConfig
      'False
      'False
      'False
      (MShared cfg)

{- | Constraint representing a well-behaved shared config.
This class holds for every possible @AvalonMmSharedConfig@,
but we need to write out the class anyway so that GHC holds.
-}
type KnownSharedConfig config =
  ( KnownNat (DataWidth config)
  , KeepTypeClass (KeepReadData config)
  , KeepTypeClass (KeepWriteData config)
  , KnownNat (AddrWidth config)
  , KeepTypeClass (KeepRead config)
  , KeepTypeClass (KeepWrite config)
  , KnownNat (ByteEnableWidth config)
  , KeepTypeClass (KeepByteEnable config)
  , KnownNat (BurstCountWidth config)
  , KeepTypeClass (KeepBurstCount config)
  , KeepTypeClass (KeepReadDataValid config)
  , KeepTypeClass (KeepEndOfPacket config)
  , NFDataX (KeepType (KeepReadData config) (Unsigned (DataWidth config)))
  , NFData (KeepType (KeepReadData config) (Unsigned (DataWidth config)))
  , ShowX (KeepType (KeepReadData config) (Unsigned (DataWidth config)))
  , Show (KeepType (KeepReadData config) (Unsigned (DataWidth config)))
  , Eq (KeepType (KeepReadData config) (Unsigned (DataWidth config)))
  , NFDataX (KeepType (KeepWriteData config) (Unsigned (DataWidth config)))
  , NFData (KeepType (KeepWriteData config) (Unsigned (DataWidth config)))
  , ShowX (KeepType (KeepWriteData config) (Unsigned (DataWidth config)))
  , Show (KeepType (KeepWriteData config) (Unsigned (DataWidth config)))
  , Eq (KeepType (KeepWriteData config) (Unsigned (DataWidth config)))
  , NFDataX (KeepType (KeepByteEnable config) (Unsigned (ByteEnableWidth config)))
  , NFData (KeepType (KeepByteEnable config) (Unsigned (ByteEnableWidth config)))
  , ShowX (KeepType (KeepByteEnable config) (Unsigned (ByteEnableWidth config)))
  , Show (KeepType (KeepByteEnable config) (Unsigned (ByteEnableWidth config)))
  , Eq (KeepType (KeepByteEnable config) (Unsigned (ByteEnableWidth config)))
  , NFDataX (KeepType (KeepBurstCount config) (Unsigned (BurstCountWidth config)))
  , NFData (KeepType (KeepBurstCount config) (Unsigned (BurstCountWidth config)))
  , ShowX (KeepType (KeepBurstCount config) (Unsigned (BurstCountWidth config)))
  , Show (KeepType (KeepBurstCount config) (Unsigned (BurstCountWidth config)))
  , Eq (KeepType (KeepBurstCount config) (Unsigned (BurstCountWidth config)))
  )

{- | Constraint representing a well-behaved subordinate config.
This class holds for every possible @AvalonMmSubordinateConfig@,
but we need to write out the class anyway so that GHC holds.
-}
type KnownSubordinateConfig config =
  ( KeepTypeClass (KeepAddr config)
  , KeepTypeClass (KeepWriteByteEnable config)
  , KeepTypeClass (KeepChipSelect config)
  , KeepTypeClass (KeepBeginTransfer config)
  , KeepTypeClass (KeepWaitRequest config)
  , KeepTypeClass (KeepBeginBurstTransfer config)
  , KeepTypeClass (KeepReadyForData config)
  , KeepTypeClass (KeepDataAvailable config)
  , KeepTypeClass (KeepIrq config)
  , KnownSharedConfig (SShared config)
  , NFDataX (KeepType (KeepAddr config) (Unsigned (AddrWidth (SShared config))))
  , NFData (KeepType (KeepAddr config) (Unsigned (AddrWidth (SShared config))))
  , ShowX (KeepType (KeepAddr config) (Unsigned (AddrWidth (SShared config))))
  , Show (KeepType (KeepAddr config) (Unsigned (AddrWidth (SShared config))))
  , Eq (KeepType (KeepAddr config) (Unsigned (AddrWidth (SShared config))))
  , NFDataX
      (KeepType (KeepWriteByteEnable config) (Unsigned (ByteEnableWidth (SShared config))))
  , NFData
      (KeepType (KeepWriteByteEnable config) (Unsigned (ByteEnableWidth (SShared config))))
  , ShowX
      (KeepType (KeepWriteByteEnable config) (Unsigned (ByteEnableWidth (SShared config))))
  , Show
      (KeepType (KeepWriteByteEnable config) (Unsigned (ByteEnableWidth (SShared config))))
  , Eq
      (KeepType (KeepWriteByteEnable config) (Unsigned (ByteEnableWidth (SShared config))))
  )

{- | Constraint representing a well-behaved manager config.
This class holds for every possible @AvalonMmManagerConfig@,
but we need to write out the class anyway so that GHC holds.
-}
type KnownManagerConfig config =
  ( KeepTypeClass (KeepFlush config)
  , KeepTypeClass (KeepIrqList config)
  , KeepTypeClass (KeepIrqNumber config)
  , KnownSharedConfig (MShared config)
  , NFDataX (KeepType (KeepIrqList config) (Unsigned 32))
  , NFData (KeepType (KeepIrqList config) (Unsigned 32))
  , ShowX (KeepType (KeepIrqList config) (Unsigned 32))
  , Show (KeepType (KeepIrqList config) (Unsigned 32))
  , Eq (KeepType (KeepIrqList config) (Unsigned 32))
  , NFDataX (KeepType (KeepIrqNumber config) (Maybe (Unsigned 6)))
  , NFData (KeepType (KeepIrqNumber config) (Maybe (Unsigned 6)))
  , ShowX (KeepType (KeepIrqNumber config) (Maybe (Unsigned 6)))
  , Show (KeepType (KeepIrqNumber config) (Maybe (Unsigned 6)))
  , Eq (KeepType (KeepIrqNumber config) (Maybe (Unsigned 6)))
  )

{- | Data coming out of an Avalon MM manager port.
All fields are optional and can be toggled using the config.
-}
data AvalonManagerOut config = AvalonManagerOut
  { mo_writeData ::
      KeepType
        (KeepWriteData (MShared config))
        (Unsigned (DataWidth (MShared config)))
  , mo_addr :: Unsigned (AddrWidth (MShared config))
  , mo_read :: KeepType (KeepRead (MShared config)) Bool
  , mo_write :: KeepType (KeepWrite (MShared config)) Bool
  , mo_byteEnable ::
      KeepType
        (KeepByteEnable (MShared config))
        (Unsigned (ByteEnableWidth (MShared config)))
  , mo_burstCount ::
      KeepType
        (KeepBurstCount (MShared config))
        (Unsigned (BurstCountWidth (MShared config)))
  , mo_flush :: KeepType (KeepFlush config) Bool
  }
  deriving (Generic, Bundle)

deriving instance
  (KnownManagerConfig config) =>
  NFDataX (AvalonManagerOut config)
deriving instance
  (KnownManagerConfig config) =>
  NFData (AvalonManagerOut config)
deriving instance
  (KnownManagerConfig config) =>
  ShowX (AvalonManagerOut config)
deriving instance
  (KnownManagerConfig config) =>
  Show (AvalonManagerOut config)
deriving instance
  (KnownManagerConfig config) =>
  Eq (AvalonManagerOut config)

{- | Data coming into an Avalon MM manager port.
Almost all fields are optional and can be toggled using the config.
WaitRequest is mandatory.
-}
data AvalonManagerIn config = AvalonManagerIn
  { mi_readData ::
      KeepType
        (KeepReadData (MShared config))
        (Unsigned (DataWidth (MShared config)))
  , mi_waitRequest :: Bool
  , mi_readDataValid :: KeepType (KeepReadDataValid (MShared config)) Bool
  , mi_endOfPacket :: KeepType (KeepEndOfPacket (MShared config)) Bool
  , mi_irqList :: KeepType (KeepIrqList config) (Unsigned 32)
  , mi_irqNumber :: KeepType (KeepIrqNumber config) (Maybe (Unsigned 6))
  }
  deriving (Generic, Bundle)

deriving instance
  (KnownManagerConfig config) =>
  NFDataX (AvalonManagerIn config)
deriving instance
  (KnownManagerConfig config) =>
  NFData (AvalonManagerIn config)
deriving instance
  (KnownManagerConfig config) =>
  ShowX (AvalonManagerIn config)
deriving instance
  (KnownManagerConfig config) =>
  Show (AvalonManagerIn config)
deriving instance
  (KnownManagerConfig config) =>
  Eq (AvalonManagerIn config)

{- | Data coming out of an Avalon MM subordinate port.
All fields are optional and can be toggled using the config.
-}
data AvalonSubordinateOut config = AvalonSubordinateOut
  { so_readData ::
      KeepType
        (KeepReadData (SShared config))
        (Unsigned (DataWidth (SShared config)))
  , so_readDataValid :: KeepType (KeepReadDataValid (SShared config)) Bool
  , so_endOfPacket :: KeepType (KeepEndOfPacket (SShared config)) Bool
  , so_waitRequest :: KeepType (KeepWaitRequest config) Bool
  , so_readyForData :: KeepType (KeepReadyForData config) Bool
  , so_dataAvailable :: KeepType (KeepDataAvailable config) Bool
  , so_irq :: KeepType (KeepIrq config) Bool
  }
  deriving (Generic, Bundle)

deriving instance
  (KnownSubordinateConfig config) =>
  NFDataX (AvalonSubordinateOut config)
deriving instance
  (KnownSubordinateConfig config) =>
  NFData (AvalonSubordinateOut config)
deriving instance
  (KnownSubordinateConfig config) =>
  ShowX (AvalonSubordinateOut config)
deriving instance
  (KnownSubordinateConfig config) =>
  Show (AvalonSubordinateOut config)
deriving instance
  (KnownSubordinateConfig config) =>
  Eq (AvalonSubordinateOut config)

{- | Data coming into an Avalon MM subordinate port.
All fields are optional and can be toggled using the config.
-}
data AvalonSubordinateIn config = AvalonSubordinateIn
  { si_writeData ::
      KeepType
        (KeepWriteData (SShared config))
        (Unsigned (DataWidth (SShared config)))
  , si_addr :: KeepType (KeepAddr config) (Unsigned (AddrWidth (SShared config)))
  , si_read :: KeepType (KeepRead (SShared config)) Bool
  , si_write :: KeepType (KeepWrite (SShared config)) Bool
  , si_byteEnable ::
      KeepType
        (KeepByteEnable (SShared config))
        (Unsigned (ByteEnableWidth (SShared config)))
  , si_burstCount ::
      KeepType
        (KeepBurstCount (SShared config))
        (Unsigned (BurstCountWidth (SShared config)))
  , si_writeByteEnable ::
      KeepType
        (KeepWriteByteEnable config)
        (Unsigned (ByteEnableWidth (SShared config)))
  , si_chipSelect :: KeepType (KeepChipSelect config) Bool
  , si_beginTransfer :: KeepType (KeepBeginTransfer config) Bool
  , si_beginBurstTransfer :: KeepType (KeepBeginBurstTransfer config) Bool
  }
  deriving (Generic, Bundle)

deriving instance
  (KnownSubordinateConfig config) =>
  NFDataX (AvalonSubordinateIn config)
deriving instance
  (KnownSubordinateConfig config) =>
  NFData (AvalonSubordinateIn config)
deriving instance
  (KnownSubordinateConfig config) =>
  ShowX (AvalonSubordinateIn config)
deriving instance
  (KnownSubordinateConfig config) =>
  Show (AvalonSubordinateIn config)
deriving instance
  (KnownSubordinateConfig config) =>
  Eq (AvalonSubordinateIn config)

{- | "Important" data pertaining to write transfers.
Can be grabbed from 'AvalonManagerOut' or 'AvalonSubordinateIn'.
-}
data AvalonWriteImpt keepAddr config = AvalonWriteImpt
  { wi_writeData ::
      KeepType
        (KeepWriteData config)
        (Unsigned (DataWidth config))
  , wi_addr :: KeepType keepAddr (Unsigned (AddrWidth config))
  , wi_byteEnable ::
      KeepType
        (KeepByteEnable config)
        (Unsigned (ByteEnableWidth config))
  , wi_burstCount ::
      KeepType
        (KeepBurstCount config)
        (Unsigned (BurstCountWidth config))
  }
  deriving (Generic, Bundle)

deriving instance
  ( KnownSharedConfig config
  , NFDataX (KeepType keepAddr (Unsigned (AddrWidth config)))
  , KeepTypeClass keepAddr
  ) =>
  NFDataX (AvalonWriteImpt keepAddr config)
deriving instance
  ( KnownSharedConfig config
  , NFData (KeepType keepAddr (Unsigned (AddrWidth config)))
  , KeepTypeClass keepAddr
  ) =>
  NFData (AvalonWriteImpt keepAddr config)
deriving instance
  ( KnownSharedConfig config
  , ShowX (KeepType keepAddr (Unsigned (AddrWidth config)))
  , KeepTypeClass keepAddr
  ) =>
  ShowX (AvalonWriteImpt keepAddr config)
deriving instance
  ( KnownSharedConfig config
  , Show (KeepType keepAddr (Unsigned (AddrWidth config)))
  , KeepTypeClass keepAddr
  ) =>
  Show (AvalonWriteImpt keepAddr config)
deriving instance
  ( KnownSharedConfig config
  , Eq (KeepType keepAddr (Unsigned (AddrWidth config)))
  , KeepTypeClass keepAddr
  ) =>
  Eq (AvalonWriteImpt keepAddr config)

{- | "Important" data pertaining to read requests.
Can be grabbed from 'AvalonManagerOut' or 'AvalonSubordinateIn'.
-}
data AvalonReadReqImpt keepAddr config = AvalonReadReqImpt
  { rri_addr :: KeepType keepAddr (Unsigned (AddrWidth config))
  , rri_byteEnable ::
      KeepType
        (KeepByteEnable config)
        (Unsigned (ByteEnableWidth config))
  , rri_burstCount ::
      KeepType
        (KeepBurstCount config)
        (Unsigned (BurstCountWidth config))
  }
  deriving (Generic, Bundle)

deriving instance
  ( KnownSharedConfig config
  , NFDataX (KeepType keepAddr (Unsigned (AddrWidth config)))
  , KeepTypeClass keepAddr
  ) =>
  NFDataX (AvalonReadReqImpt keepAddr config)
deriving instance
  ( KnownSharedConfig config
  , NFData (KeepType keepAddr (Unsigned (AddrWidth config)))
  , KeepTypeClass keepAddr
  ) =>
  NFData (AvalonReadReqImpt keepAddr config)
deriving instance
  ( KnownSharedConfig config
  , ShowX (KeepType keepAddr (Unsigned (AddrWidth config)))
  , KeepTypeClass keepAddr
  ) =>
  ShowX (AvalonReadReqImpt keepAddr config)
deriving instance
  ( KnownSharedConfig config
  , Show (KeepType keepAddr (Unsigned (AddrWidth config)))
  , KeepTypeClass keepAddr
  ) =>
  Show (AvalonReadReqImpt keepAddr config)
deriving instance
  ( KnownSharedConfig config
  , Eq (KeepType keepAddr (Unsigned (AddrWidth config)))
  , KeepTypeClass keepAddr
  ) =>
  Eq (AvalonReadReqImpt keepAddr config)

{- | "Important" data pertaining to subordinate-to-master read data.
Can be grabbed from 'AvalonManagerIn' or 'AvalonSubordinateOut'.
-}
data AvalonReadImpt config = AvalonReadImpt
  { ri_readData ::
      KeepType
        (KeepReadData config)
        (Unsigned (DataWidth config))
  , ri_endOfPacket :: KeepType (KeepEndOfPacket config) Bool
  }
  deriving (Generic, Bundle)

deriving instance
  (KnownSharedConfig config) =>
  NFDataX (AvalonReadImpt config)
deriving instance
  (KnownSharedConfig config) =>
  NFData (AvalonReadImpt config)
deriving instance
  (KnownSharedConfig config) =>
  ShowX (AvalonReadImpt config)
deriving instance
  (KnownSharedConfig config) =>
  Show (AvalonReadImpt config)
deriving instance
  (KnownSharedConfig config) =>
  Eq (AvalonReadImpt config)

{- | Given an 'AvalonManagerOut', separate out the parts which are not
compatible with 'DfConv.DfConv'.
-}
managerOutRemoveNonDf ::
  (KnownManagerConfig cfg) =>
  AvalonManagerOut cfg ->
  ( AvalonManagerOut (RemoveNonDfManager cfg)
  , KeepType (KeepFlush cfg) Bool
  )
managerOutRemoveNonDf AvalonManagerOut{..} =
  ( AvalonManagerOut
      { mo_addr
      , mo_read
      , mo_write
      , mo_byteEnable
      , mo_burstCount
      , mo_writeData
      , mo_flush = keepTypeFalse
      }
  , mo_flush
  )

{- | Given an 'AvalonManagerOut' which is compatible with 'DfConv.DfConv', add
back in the parts which are not compatible with 'DfConv.DfConv'.
-}
managerOutAddNonDf ::
  (KnownManagerConfig cfg) =>
  KeepType (KeepFlush cfg) Bool ->
  AvalonManagerOut (RemoveNonDfManager cfg) ->
  AvalonManagerOut cfg
managerOutAddNonDf flush AvalonManagerOut{..} =
  AvalonManagerOut
    { mo_addr
    , mo_read
    , mo_write
    , mo_byteEnable
    , mo_burstCount
    , mo_writeData
    , mo_flush = flush
    }

{- | Given an 'AvalonManagerIn', separate out the parts which are not
compatible with 'DfConv.DfConv'.
-}
managerInRemoveNonDf ::
  (KnownManagerConfig cfg) =>
  AvalonManagerIn cfg ->
  ( AvalonManagerIn (RemoveNonDfManager cfg)
  , ( KeepType (KeepIrqList cfg) (Unsigned 32)
    , KeepType (KeepIrqNumber cfg) (Maybe (Unsigned 6))
    )
  )
managerInRemoveNonDf AvalonManagerIn{..} =
  ( AvalonManagerIn
      { mi_readData
      , mi_waitRequest
      , mi_readDataValid
      , mi_endOfPacket
      , mi_irqList = keepTypeFalse
      , mi_irqNumber = keepTypeFalse
      }
  , (mi_irqList, mi_irqNumber)
  )

{- | Given an 'AvalonManagerIn' which is compatible with 'DfConv.DfConv', add
back in the parts which are not compatible with 'DfConv.DfConv'.
-}
managerInAddNonDf ::
  (KnownManagerConfig cfg) =>
  ( KeepType (KeepIrqList cfg) (Unsigned 32)
  , KeepType (KeepIrqNumber cfg) (Maybe (Unsigned 6))
  ) ->
  AvalonManagerIn (RemoveNonDfManager cfg) ->
  AvalonManagerIn cfg
managerInAddNonDf (irqList, irqNumber) AvalonManagerIn{..} =
  AvalonManagerIn
    { mi_readData
    , mi_waitRequest
    , mi_readDataValid
    , mi_endOfPacket
    , mi_irqList = irqList
    , mi_irqNumber = irqNumber
    }

{- | Given an 'AvalonSubordinateOut', separate out the parts which are not
compatible with 'DfConv.DfConv'.
-}
subordinateOutRemoveNonDf ::
  (KnownSubordinateConfig cfg) =>
  AvalonSubordinateOut cfg ->
  ( AvalonSubordinateOut (RemoveNonDfSubordinate cfg)
  , ( KeepType (KeepReadyForData cfg) Bool
    , KeepType (KeepDataAvailable cfg) Bool
    , KeepType (KeepIrq cfg) Bool
    )
  )
subordinateOutRemoveNonDf AvalonSubordinateOut{..} =
  ( AvalonSubordinateOut
      { so_waitRequest
      , so_readDataValid
      , so_endOfPacket
      , so_readData
      , so_readyForData = keepTypeFalse
      , so_dataAvailable = keepTypeFalse
      , so_irq = keepTypeFalse
      }
  , (so_readyForData, so_dataAvailable, so_irq)
  )

{- | Given an 'AvalonSubordinateOut' which is compatible with 'DfConv.DfConv',
add back in the parts which are not compatible with 'DfConv.DfConv'.
-}
subordinateOutAddNonDf ::
  (KnownSubordinateConfig cfg) =>
  ( KeepType (KeepReadyForData cfg) Bool
  , KeepType (KeepDataAvailable cfg) Bool
  , KeepType (KeepIrq cfg) Bool
  ) ->
  AvalonSubordinateOut (RemoveNonDfSubordinate cfg) ->
  AvalonSubordinateOut cfg
subordinateOutAddNonDf (readyForData, dataAvailable, irq) AvalonSubordinateOut{..} =
  AvalonSubordinateOut
    { so_waitRequest
    , so_readDataValid
    , so_endOfPacket
    , so_readData
    , so_readyForData = readyForData
    , so_dataAvailable = dataAvailable
    , so_irq = irq
    }

{- | Given an 'AvalonSubordinateIn', separate out the parts which are not
compatible with 'DfConv.DfConv'.
-}
subordinateInRemoveNonDf ::
  (KnownSubordinateConfig cfg) =>
  AvalonSubordinateIn cfg ->
  ( AvalonSubordinateIn (RemoveNonDfSubordinate cfg)
  , ( KeepType (KeepBeginBurstTransfer cfg) Bool
    , KeepType (KeepBeginTransfer cfg) Bool
    )
  )
subordinateInRemoveNonDf AvalonSubordinateIn{..} =
  ( AvalonSubordinateIn
      { si_chipSelect
      , si_addr
      , si_read
      , si_write
      , si_byteEnable
      , si_writeByteEnable
      , si_burstCount
      , si_writeData
      , si_beginBurstTransfer = keepTypeFalse
      , si_beginTransfer = keepTypeFalse
      }
  , (si_beginBurstTransfer, si_beginTransfer)
  )

{- | Given an 'AvalonSubordinateIn' which is compatible with 'DfConv.DfConv',
add back in the parts which are not compatible with 'DfConv.DfConv'.
-}
subordinateInAddNonDf ::
  (KnownSubordinateConfig cfg) =>
  ( KeepType (KeepBeginBurstTransfer cfg) Bool
  , KeepType (KeepBeginTransfer cfg) Bool
  ) ->
  AvalonSubordinateIn (RemoveNonDfSubordinate cfg) ->
  AvalonSubordinateIn cfg
subordinateInAddNonDf (beginBurstTransfer, beginTransfer) AvalonSubordinateIn{..} =
  AvalonSubordinateIn
    { si_chipSelect
    , si_addr
    , si_read
    , si_write
    , si_byteEnable
    , si_writeByteEnable
    , si_burstCount
    , si_writeData
    , si_beginBurstTransfer = beginBurstTransfer
    , si_beginTransfer = beginTransfer
    }

{- | Convert a boolean value to an @AvalonSubordinateOut@ structure.
The structure gives no read data, no IRQ, etc. Fields relating to
"acknowledging" a write, or a read request, are controlled by the bool input.
-}
boolToMmSubordinateAck ::
  (KnownSubordinateConfig config) => Bool -> AvalonSubordinateOut config
boolToMmSubordinateAck ack =
  AvalonSubordinateOut
    { so_waitRequest = toKeepType (not ack)
    , so_readDataValid = toKeepType False
    , so_readyForData = toKeepType ack
    , so_dataAvailable = toKeepType False
    , so_endOfPacket = toKeepType False
    , so_irq = toKeepType False
    , so_readData = errorX "No readData for boolToAck"
    }

{- | Convert a boolean value to an @AvalonManagerIn@ structure.
The structure gives no read data, no IRQ, etc. The @waitRequest@ field is
controlled by the (negated) boolean input.
-}
boolToMmManagerAck ::
  (KnownManagerConfig config) => Bool -> AvalonManagerIn config
boolToMmManagerAck ack =
  AvalonManagerIn
    { mi_waitRequest = not ack
    , mi_readDataValid = toKeepType False
    , mi_endOfPacket = toKeepType False
    , mi_irqList = toKeepType 0
    , mi_irqNumber = toKeepType Nothing
    , mi_readData = errorX "No readData for boolToAck"
    }

{- | Convert an 'AvalonReadImpt' into an 'AvalonSubordinateOut' containing that
read data.
-}
mmReadImptToSubordinateOut ::
  (KnownSubordinateConfig config, config ~ RemoveNonDfSubordinate config) =>
  AvalonReadImpt (SShared config) ->
  AvalonSubordinateOut config
mmReadImptToSubordinateOut dat =
  AvalonSubordinateOut
    { so_waitRequest = toKeepType False
    , so_readDataValid = toKeepType True
    , so_readyForData = keepTypeFalse
    , so_dataAvailable = keepTypeFalse
    , so_endOfPacket = ri_endOfPacket dat
    , so_irq = keepTypeFalse
    , so_readData = ri_readData dat
    }

{- | Grab the important read information from an 'AvalonSubordinateOut', putting
it into an 'AvalonReadImpt'. Only works if fixed wait time is 0.
-}
mmSubordinateOutToReadImpt ::
  (KnownSubordinateConfig config, config ~ RemoveNonDfSubordinate config) =>
  AvalonSubordinateOut config ->
  Maybe (AvalonReadImpt (SShared config))
mmSubordinateOutToReadImpt (AvalonSubordinateOut{..})
  | cond =
      Just
        AvalonReadImpt
          { ri_endOfPacket = so_endOfPacket
          , ri_readData = so_readData
          }
  | otherwise = Nothing
 where
  cond =
    fromKeepTypeDef True so_readDataValid
      && not (fromKeepTypeDef False so_waitRequest)

{- | Convert an 'AvalonReadImpt' into an 'AvalonManagerIn' containing that
read data.
-}
mmReadImptToManagerIn ::
  (KnownManagerConfig config, config ~ RemoveNonDfManager config) =>
  AvalonReadImpt (MShared config) ->
  AvalonManagerIn config
mmReadImptToManagerIn dat =
  AvalonManagerIn
    { mi_waitRequest = False
    , mi_readDataValid = toKeepType True
    , mi_endOfPacket = ri_endOfPacket dat
    , mi_irqList = keepTypeFalse
    , mi_irqNumber = keepTypeFalse
    , mi_readData = ri_readData dat
    }

{- | Grab the important read information from an 'AvalonManagerIn', putting it
into an 'AvalonReadImpt'. Only works if fixed wait time is 0.
-}
mmManagerInToReadImpt ::
  (KnownManagerConfig config, config ~ RemoveNonDfManager config) =>
  AvalonManagerIn config ->
  Maybe (AvalonReadImpt (MShared config))
mmManagerInToReadImpt (AvalonManagerIn{..})
  | cond =
      Just
        AvalonReadImpt
          { ri_endOfPacket = mi_endOfPacket
          , ri_readData = mi_readData
          }
  | otherwise = Nothing
 where
  cond = fromKeepTypeDef True mi_readDataValid

{- | Convert an 'AvalonWriteImpt' into an 'AvalonSubordinateIn' containing
that write data.
-}
mmWriteImptToSubordinateIn ::
  (KnownSubordinateConfig config, config ~ RemoveNonDfSubordinate config) =>
  AvalonWriteImpt (KeepAddr config) (SShared config) ->
  AvalonSubordinateIn config
mmWriteImptToSubordinateIn (AvalonWriteImpt{..}) =
  AvalonSubordinateIn
    { si_chipSelect = toKeepType True
    , si_addr = wi_addr
    , si_read = toKeepType False
    , si_write = toKeepType True
    , si_byteEnable = wi_byteEnable
    , si_writeByteEnable =
        convKeepType
          (bitCoerce $ repeat True)
          wi_byteEnable
    , si_beginTransfer = keepTypeFalse
    , si_burstCount = wi_burstCount
    , si_beginBurstTransfer = keepTypeFalse
    , si_writeData = wi_writeData
    }

{- | Grab the important write information from an 'AvalonSubordinateIn',
putting it into an 'AvalonWriteImpt'.
-}
mmSubordinateInToWriteImpt ::
  (KnownSubordinateConfig config, config ~ RemoveNonDfSubordinate config) =>
  AvalonSubordinateIn config ->
  Maybe (AvalonWriteImpt (KeepAddr config) (SShared config))
mmSubordinateInToWriteImpt (AvalonSubordinateIn{..})
  | cond =
      Just
        AvalonWriteImpt
          { wi_addr = si_addr
          , wi_byteEnable = si_byteEnable
          , wi_burstCount = si_burstCount
          , wi_writeData = si_writeData
          }
  | otherwise = Nothing
 where
  cond =
    fromKeepTypeDef True si_chipSelect
      && fromKeepTypeDef True si_write
      && not (fromKeepTypeDef False si_read)

{- | Convert an 'AvalonWriteImpt' into an 'AvalonManagerOut' containing that
write data.
-}
mmWriteImptToManagerOut ::
  (KnownManagerConfig config, config ~ RemoveNonDfManager config) =>
  AvalonWriteImpt 'True (MShared config) ->
  AvalonManagerOut config
mmWriteImptToManagerOut (AvalonWriteImpt{..}) =
  AvalonManagerOut
    { mo_addr = fromKeepTypeTrue wi_addr
    , mo_read = toKeepType False
    , mo_write = toKeepType True
    , mo_byteEnable = wi_byteEnable
    , mo_burstCount = wi_burstCount
    , mo_flush = keepTypeFalse
    , mo_writeData = wi_writeData
    }

{- | Grab the important write information from an 'AvalonManagerOut', putting
it into an 'AvalonWriteImpt'.
-}
mmManagerOutToWriteImpt ::
  (KnownManagerConfig config, config ~ RemoveNonDfManager config) =>
  AvalonManagerOut config ->
  Maybe (AvalonWriteImpt 'True (MShared config))
mmManagerOutToWriteImpt (AvalonManagerOut{..})
  | cond =
      Just
        AvalonWriteImpt
          { wi_addr = toKeepType mo_addr
          , wi_byteEnable = mo_byteEnable
          , wi_burstCount = mo_burstCount
          , wi_writeData = mo_writeData
          }
  | otherwise = Nothing
 where
  cond =
    fromKeepTypeDef True mo_write
      && not (fromKeepTypeDef False mo_read)

{- | Convert an 'AvalonReadReqImpt' into an 'AvalonSubordinateIn' containing
that read request data.
-}
mmReadReqImptToSubordinateIn ::
  (KnownSubordinateConfig config, config ~ RemoveNonDfSubordinate config) =>
  AvalonReadReqImpt (KeepAddr config) (SShared config) ->
  AvalonSubordinateIn config
mmReadReqImptToSubordinateIn (AvalonReadReqImpt{..}) =
  AvalonSubordinateIn
    { si_chipSelect = toKeepType True
    , si_addr = rri_addr
    , si_read = toKeepType True
    , si_write = toKeepType False
    , si_byteEnable = rri_byteEnable
    , si_writeByteEnable = toKeepType 0
    , si_beginTransfer = keepTypeFalse
    , si_burstCount = rri_burstCount
    , si_beginBurstTransfer = keepTypeFalse
    , si_writeData = errorX "No writeData for read req"
    }

{- | Grab the important read request information from an 'AvalonSubordinateIn',
putting it into an 'AvalonReadReqImpt'.
-}
mmSubordinateInToReadReqImpt ::
  (KnownSubordinateConfig config, config ~ RemoveNonDfSubordinate config) =>
  AvalonSubordinateIn config ->
  Maybe (AvalonReadReqImpt (KeepAddr config) (SShared config))
mmSubordinateInToReadReqImpt (AvalonSubordinateIn{..})
  | cond =
      Just
        AvalonReadReqImpt
          { rri_addr = si_addr
          , rri_byteEnable = si_byteEnable
          , rri_burstCount = si_burstCount
          }
  | otherwise = Nothing
 where
  cond =
    fromKeepTypeDef True si_chipSelect
      && fromKeepTypeDef True si_read
      && not (fromKeepTypeDef False si_write)

{- | Convert an 'AvalonReadReqImpt' into an 'AvalonManagerOut' containing that
read request data.
-}
mmReadReqImptToManagerOut ::
  (KnownManagerConfig config, config ~ RemoveNonDfManager config) =>
  AvalonReadReqImpt 'True (MShared config) ->
  AvalonManagerOut config
mmReadReqImptToManagerOut (AvalonReadReqImpt{..}) =
  AvalonManagerOut
    { mo_addr = fromKeepTypeTrue rri_addr
    , mo_read = toKeepType True
    , mo_write = toKeepType False
    , mo_byteEnable = rri_byteEnable
    , mo_burstCount = rri_burstCount
    , mo_flush = keepTypeFalse
    , mo_writeData = errorX "No writeData for read req"
    }

{- | Grab the important read request information from an 'AvalonManagerOut',
putting it into an 'AvalonReadReqImpt'.
-}
mmManagerOutToReadReqImpt ::
  (KnownManagerConfig config, config ~ RemoveNonDfManager config) =>
  AvalonManagerOut config ->
  Maybe (AvalonReadReqImpt 'True (MShared config))
mmManagerOutToReadReqImpt (AvalonManagerOut{..})
  | cond =
      Just
        AvalonReadReqImpt
          { rri_addr = toKeepType mo_addr
          , rri_byteEnable = mo_byteEnable
          , rri_burstCount = mo_burstCount
          }
  | otherwise = Nothing
 where
  cond =
    fromKeepTypeDef True mo_read
      && not (fromKeepTypeDef False mo_write)

{- | An 'AvalonSubordinateIn' containing no write data, and indicating that no
transmission is currently occurring.
-}
mmSubordinateInNoData ::
  (KnownSubordinateConfig config) =>
  AvalonSubordinateIn config
mmSubordinateInNoData =
  AvalonSubordinateIn
    { si_chipSelect = toKeepType False
    , si_addr = toKeepType 0
    , si_read = toKeepType False
    , si_write = toKeepType False
    , si_byteEnable = toKeepType 0
    , si_writeByteEnable = toKeepType 0
    , si_beginTransfer = toKeepType False
    , si_burstCount = toKeepType 0
    , si_beginBurstTransfer = toKeepType False
    , si_writeData = errorX "No writeData for noData"
    }

{- | An 'AvalonManagerOut' containing no write data, and indicating that no
transmission is currently occurring.
-}
mmManagerOutNoData :: (KnownManagerConfig config) => AvalonManagerOut config
mmManagerOutNoData =
  AvalonManagerOut
    { mo_addr = 0
    , mo_read = toKeepType False
    , mo_write = toKeepType False
    , mo_byteEnable = toKeepType 0
    , mo_burstCount = toKeepType 0
    , mo_flush = toKeepType False
    , mo_writeData = errorX "No writeData for noData"
    }

{- | Grab the "acknowledgement" value from an 'AvalonSubordinateOut'.
Reasonable defaults are provided for optional fields.
-}
mmSubordinateOutToBool ::
  (KnownSubordinateConfig config) =>
  AvalonSubordinateOut config ->
  Bool
mmSubordinateOutToBool so =
  fromKeepTypeDef True (so_readyForData so)
    && not (fromKeepTypeDef False (so_waitRequest so))

{- | Grab the "acknowledgement" value from an 'AvalonManagerIn'.
Reasonable defaults are provided for optional fields.
-}
mmManagerInToBool ::
  (KnownManagerConfig config) => AvalonManagerIn config -> Bool
mmManagerInToBool = not . mi_waitRequest

-- | Datatype for the manager end of the Avalon memory-mapped protocol.
data AvalonMmManager (dom :: Domain) (config :: AvalonMmManagerConfig)
  = AvalonMmManager

-- | Datatype for the subordinate end of the Avalon memory-mapped protocol.
data
  AvalonMmSubordinate
    (dom :: Domain)
    (fixedWaitTime :: Nat)
    (config :: AvalonMmSubordinateConfig)
  = AvalonMmSubordinate

instance Protocol (AvalonMmManager dom config) where
  type Fwd (AvalonMmManager dom config) = Signal dom (AvalonManagerOut config)
  type Bwd (AvalonMmManager dom config) = Signal dom (AvalonManagerIn config)

instance Protocol (AvalonMmSubordinate dom fixedWaitTime config) where
  type
    Fwd (AvalonMmSubordinate dom fixedWaitTime config) =
      Signal dom (AvalonSubordinateIn config)
  type
    Bwd (AvalonMmSubordinate dom fixedWaitTime config) =
      Signal dom (AvalonSubordinateOut config)

instance
  (KnownSubordinateConfig config, KeepWaitRequest config ~ 'True) =>
  Backpressure (AvalonMmSubordinate dom 0 config)
  where
  boolsToBwd _ = C.fromList_lazy . fmap boolToMmSubordinateAck

instance
  (KnownManagerConfig config) =>
  Backpressure (AvalonMmManager dom config)
  where
  boolsToBwd _ = C.fromList_lazy . fmap boolToMmManagerAck

-- 'toDfLike' and 'fromDfLike' here are very similar to their implementations
-- in @instance DfConv.DfConv (AvalonMmManager ...)@. 'toDfCircuit' forces
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
  (KnownSubordinateConfig config, config ~ RemoveNonDfSubordinate config) =>
  DfConv.DfConv (AvalonMmSubordinate dom 0 config)
  where
  type Dom (AvalonMmSubordinate dom 0 config) = dom
  type
    BwdPayload (AvalonMmSubordinate dom 0 config) =
      AvalonReadImpt (SShared config)
  type
    FwdPayload (AvalonMmSubordinate dom 0 config) =
      Either
        (AvalonReadReqImpt (KeepAddr config) (SShared config))
        (AvalonWriteImpt (KeepAddr config) (SShared config))

  toDfCircuit proxy = DfConv.toDfCircuitHelper proxy s0 blankOtp stateFn
   where
    s0 = (Nothing, False)
    -- Nothing: readDatStored:
    --   reads only get sent for one clock cycle, so we have to store it
    --   until it's acked
    -- False: readReqAcked:
    --   when our read request is acknowledged, we should stop sending the
    --   read request forwards
    blankOtp = mmSubordinateInNoData
    stateFn so dfAck dfDat = do
      (readDatStored0, readReqAcked0) <- get
      let
        readDatIn = mmSubordinateOutToReadImpt so
        soBool = mmSubordinateOutToBool so
        (readDatStored1, readReqAcked1, si, dfAckOut) =
          genOutputs dfDat readDatStored0 readReqAcked0 readDatIn soBool
      put
        ( if ( Maybe.isNothing readDatStored1 -- to avoid looking at dfAck when not needed
                || dfAck
             )
            then Nothing
            else readDatStored1
        , readReqAcked1
        )
      pure (si, readDatStored1, dfAckOut)
     where
      genOutputs (Just (Right wi)) readDatStored0 _ _ soBool =
        ( readDatStored0
        , False
        , mmWriteImptToSubordinateIn
            (wi{wi_burstCount = toKeepType 1})
        , soBool
        )
      genOutputs _ readDatStored0@(Just _) _ _ soBool =
        ( readDatStored0
        , False
        , mmSubordinateInNoData
        , soBool
        )
      genOutputs (Just (Left ri)) Nothing readReqAcked0 readDatIn soBool =
        ( readDatIn
        , soBool
        , if readReqAcked0
            then mmSubordinateInNoData
            else
              mmReadReqImptToSubordinateIn
                (ri{rri_burstCount = toKeepType 1})
        , soBool
        )
      genOutputs Nothing Nothing _ _ _ =
        ( Nothing
        , False
        , mmSubordinateInNoData
        , False
        )

  fromDfCircuit proxy = DfConv.fromDfCircuitHelper proxy s0 blankOtp stateFn
   where
    s0 = False -- dfAckSt -- read request might be acked before read is sent back
    blankOtp = boolToMmSubordinateAck False
    stateFn si dfAck dfDat = do
      dfAckSt <- get
      let
        writeImpt = mmSubordinateInToWriteImpt si
        readReqImpt
          | Nothing <- writeImpt = mmSubordinateInToReadReqImpt si
          | otherwise = Nothing
        sendingReadDat
          | Just _ <- readReqImpt, dfAckSt = dfDat
          | otherwise = Nothing
        dfAckSt' = Maybe.isJust readReqImpt && (dfAckSt || dfAck)
        so = case (writeImpt, sendingReadDat) of
          (Just _, _) -> boolToMmSubordinateAck dfAck
          (_, Just rdat) -> mmReadImptToSubordinateOut rdat
          _ -> boolToMmSubordinateAck False
        dfDatOut = case (writeImpt, readReqImpt, dfAckSt) of
          (Just wi, _, _) -> Just (Right wi)
          (_, Just ri, False) -> Just (Left ri)
          _ -> Nothing
      let dfAckOut = Maybe.isJust sendingReadDat
      put dfAckSt'
      pure (so, dfDatOut, dfAckOut)

-- See comments for @instance DfConv.DfConv (AvalonMmSubordinate ...)@, as the
-- instance is very similar and the comments there apply here as well.
instance
  (KnownManagerConfig config, config ~ RemoveNonDfManager config) =>
  DfConv.DfConv (AvalonMmManager dom config)
  where
  type Dom (AvalonMmManager dom config) = dom
  type BwdPayload (AvalonMmManager dom config) = AvalonReadImpt (MShared config)
  type
    FwdPayload (AvalonMmManager dom config) =
      Either
        (AvalonReadReqImpt 'True (MShared config))
        (AvalonWriteImpt 'True (MShared config))

  toDfCircuit proxy = DfConv.toDfCircuitHelper proxy s0 blankOtp stateFn
   where
    s0 = (Nothing, False)
    -- Nothing: readDatStored:
    --   reads only get sent for one clock cycle, so we have to store it
    --   until it's acked
    -- False: readReqAcked:
    --   when our read request is acknowledged, we should stop sending the
    --   read request forwards
    blankOtp = mmManagerOutNoData
    stateFn mi dfAck dfDat = do
      (readDatStored0, readReqAcked0) <- get
      let
        readDatIn = mmManagerInToReadImpt mi
        miBool = mmManagerInToBool mi
        (readDatStored1, readReqAcked1, mo, dfAckOut) =
          genOutputs dfDat readDatStored0 readReqAcked0 readDatIn miBool
      put
        ( if ( Maybe.isNothing readDatStored1 -- to avoid looking at dfAck when not needed
                || dfAck
             )
            then Nothing
            else readDatStored1
        , readReqAcked1
        )
      pure (mo, readDatStored1, dfAckOut)
     where
      genOutputs (Just (Right wi)) readDatStored0 _ _ miBool =
        ( readDatStored0
        , False
        , mmWriteImptToManagerOut
            (wi{wi_burstCount = toKeepType 1})
        , miBool
        )
      genOutputs _ readDatStored0@(Just _) _ _ miBool =
        ( readDatStored0
        , False
        , mmManagerOutNoData
        , miBool
        )
      genOutputs (Just (Left ri)) Nothing readReqAcked0 readDatIn miBool =
        ( readDatIn
        , miBool
        , if readReqAcked0
            then mmManagerOutNoData
            else
              mmReadReqImptToManagerOut
                (ri{rri_burstCount = toKeepType 1})
        , miBool
        )
      genOutputs Nothing Nothing _ _ _ =
        ( Nothing
        , False
        , mmManagerOutNoData
        , False
        )

  fromDfCircuit proxy = DfConv.fromDfCircuitHelper proxy s0 blankOtp stateFn
   where
    s0 = False -- dfAckSt -- read request might be acked before read is sent back
    blankOtp = boolToMmManagerAck False
    stateFn mo dfAck dfDat = do
      dfAckSt <- get
      let
        writeImpt = mmManagerOutToWriteImpt mo
        readReqImpt
          | Nothing <- writeImpt = mmManagerOutToReadReqImpt mo
          | otherwise = Nothing
        sendingReadDat
          | Just _ <- readReqImpt, dfAckSt = dfDat
          | otherwise = Nothing
        dfAckSt' = Maybe.isJust readReqImpt && (dfAckSt || dfAck)
        mi = case (writeImpt, sendingReadDat) of
          (Just _, _) -> boolToMmManagerAck dfAck
          (_, Just rdat) -> mmReadImptToManagerIn rdat
          _ -> boolToMmManagerAck False
        dfDatOut = case (writeImpt, readReqImpt, dfAckSt) of
          (Just wi, _, _) -> Just (Right wi)
          (_, Just ri, False) -> Just (Left ri)
          _ -> Nothing
      let dfAckOut = Maybe.isJust sendingReadDat
      put dfAckSt'
      pure (mi, dfDatOut, dfAckOut)

instance
  ( KnownManagerConfig config
  , KnownDomain dom
  , config ~ RemoveNonDfManager config
  ) =>
  Simulate (AvalonMmManager dom config)
  where
  type SimulateFwdType (AvalonMmManager dom config) = [AvalonManagerOut config]
  type SimulateBwdType (AvalonMmManager dom config) = [AvalonManagerIn config]
  type SimulateChannels (AvalonMmManager dom config) = 1

  simToSigFwd _ = fromList_lazy
  simToSigBwd _ = fromList_lazy
  sigToSimFwd _ s = sample_lazy s
  sigToSimBwd _ s = sample_lazy s

  stallC conf (head -> (stallAck, stalls)) =
    withClockResetEnable clockGen resetGen enableGen
      $ DfConv.stall Proxy Proxy conf stallAck stalls

instance
  ( KnownSubordinateConfig config
  , KnownDomain dom
  , config ~ RemoveNonDfSubordinate config
  ) =>
  Simulate (AvalonMmSubordinate dom 0 config)
  where
  type
    SimulateFwdType (AvalonMmSubordinate dom 0 config) =
      [AvalonSubordinateIn config]
  type
    SimulateBwdType (AvalonMmSubordinate dom 0 config) =
      [AvalonSubordinateOut config]
  type SimulateChannels (AvalonMmSubordinate dom 0 config) = 1

  simToSigFwd _ = fromList_lazy
  simToSigBwd _ = fromList_lazy
  sigToSimFwd _ s = sample_lazy s
  sigToSimBwd _ s = sample_lazy s

  stallC conf (head -> (stallAck, stalls)) =
    withClockResetEnable clockGen resetGen enableGen
      $ DfConv.stall Proxy Proxy conf stallAck stalls

-- NOTE: Unfortunately, we can't write a 'Drivable' instance (and, by extension,
-- a 'Test' instance) for 'AvalonMmManager' or 'AvalonMmSubordinate'.  This is
-- because they have both write data sent one way and read data sent the other.
-- By writing a 'Drivable' instance (meant for protocols with unidirectional
-- data), we would have to ignore one or the other.
--
-- Tests can still be made for Avalon MM circuits, using 'DfConv.dfConvTestBench'.
-- See 'Tests.Protocols.AvalonMemMap' for examples.

instance
  (KnownManagerConfig config) =>
  IdleCircuit (AvalonMmManager dom config)
  where
  idleFwd _ = pure mmManagerOutNoData
  idleBwd _ = pure $ boolToMmManagerAck False

instance
  (KnownSubordinateConfig config) =>
  IdleCircuit (AvalonMmSubordinate dom fixedWaitTime config)
  where
  idleFwd _ = pure mmSubordinateInNoData
  idleBwd _ = pure $ boolToMmSubordinateAck False

{- | Force a /nack/ on the backward channel and /no data/ on the forward
channel if reset is asserted.
-}
forceResetSanity ::
  (KnownDomain dom, HiddenReset dom, KnownManagerConfig config) =>
  Circuit (AvalonMmManager dom config) (AvalonMmManager dom config)
forceResetSanity = forceResetSanityGeneric
