{-|
Types and utilities shared between AXI4, AXI4-Lite, and AXI3.
-}
{-# LANGUAGE UndecidableInstances #-}

module Protocols.Axi4.Raw.Common where

-- base
import Data.Kind (Type)
import GHC.Generics (Generic)
import GHC.TypeNats (Nat)

-- clash-prelude
import qualified Clash.Prelude as C
import Clash.Prelude (type (^), type (-))

-- | Simple wrapper to achieve "named arguments" when instantiating an AXI protocol
data IdWidth = IdWidth Nat deriving (Show)

-- | Simple wrapper to achieve "named arguments" when instantiating an AXI protocol
data AddrWidth = AddrWidth Nat deriving (Show)

-- | Simple wrapper to achieve "named arguments" when instantiating an AXI protocol
data LengthWidth = LengthWidth Nat deriving (Show)

-- | Simple wrapper to achieve "named arguments" when instantiating an AXI protocol
data UserType = UserType Type KeepStrobe

-- | Keep or remove Burst, see 'BurstMode'
data KeepBurst = KeepBurst | NoBurst deriving (Show)

-- | Keep or remove Burst Length, see 'BurstSize'
data KeepBurstLength = KeepBurstLength | NoBurstLength deriving (Show)

-- | Keep or remove cache field, see 'Cache'
data KeepCache = KeepCache | NoCache deriving (Show)

-- | Keep or remove last field
data KeepLast = KeepLast | NoLast deriving (Show)

-- | Keep or remove lock, see 'AtomicAccess'
data KeepLock = KeepLock | NoLock deriving (Show)

-- | Keep or remove permissions, see 'Privileged', 'Secure', and
-- 'InstructionOrData'.
data KeepPermissions = KeepPermissions | NoPermissions deriving (Show)

-- | Keep or remove quality of service field. See 'Qos'.
data KeepQos = KeepQos | NoQos deriving (Show)

-- | Keep or remove region field
data KeepRegion = KeepRegion | NoRegion deriving (Show)

-- | Keep or remove response field. See 'Resp'.
data KeepResponse = KeepResponse | NoResponse deriving (Show)

-- | Keep or remove burst size field. See 'BurstSize'.
data KeepSize = KeepSize | NoSize deriving (Show)

-- | Keep or remove strobe field. See 'Strobe'
data KeepStrobe = KeepStrobe | NoStrobe deriving (Show)

-- | Extracts Nat from 'IdWidth', 'AddrWidth', and 'LengthWidth'
type family Width (a :: k) :: Nat where
  Width ('IdWidth n) = n
  Width ('AddrWidth n) = n
  Width ('LengthWidth n) = n

-- | Enables or disables 'BurstMode'
type family BurstType (keepBurst :: KeepBurst) where
  BurstType 'KeepBurst = BurstMode
  BurstType 'NoBurst = ()

-- | Enables or disables burst length
type family BurstLengthType (keepBurstLength :: KeepBurstLength) where
  BurstLengthType 'KeepBurstLength = C.Index (2^8)
  BurstLengthType 'NoBurstLength = ()

-- | Enables or disables 'Cache'
type family CacheType (keepCache :: KeepCache) where
  CacheType 'KeepCache = Cache
  CacheType 'NoCache = ()

-- | Enables or disables a boolean indicating whether a transaction is done
type family LastType (keepLast :: KeepLast) where
  LastType 'KeepLast = Bool
  LastType 'NoLast = ()

-- | Enables or disables 'AtomicAccess'
type family LockType (keepLockType :: KeepLock) where
  LockType 'KeepLock = AtomicAccess
  LockType 'NoLock = ()

-- | Enables or disables 'Privileged', 'Secure', and 'InstructionOrData'
type family PermissionsType (keepPermissions :: KeepPermissions) where
  PermissionsType 'KeepPermissions = (Privileged, Secure, InstructionOrData)
  PermissionsType 'NoPermissions = ()

-- | Enables or disables 'Qos'
type family QosType (keepQos :: KeepQos) where
  QosType 'KeepQos = Qos
  QosType 'NoQos = ()

-- | Enables or disables region type
type family RegionType (keepRegion :: KeepRegion) where
  RegionType 'KeepRegion = C.BitVector 4
  RegionType 'NoRegion = ()

-- | Enables or disables 'Resp'
type family ResponseType (keepResponse :: KeepResponse) where
  ResponseType 'KeepResponse = Resp
  ResponseType 'NoResponse = ()

-- | Enables or disables 'BurstSize'
type family SizeType (keepSize :: KeepSize) where
  SizeType 'KeepSize = BurstSize
  SizeType 'NoSize = ()

-- | Enable or disable 'Strobe'
type family StrobeType (byteSize :: Nat) (keepStrobe :: KeepStrobe) where
  StrobeType byteSize 'KeepStrobe = Strobe byteSize
  StrobeType byteSize 'NoStrobe = ()

-- | Indicates valid bytes on data field.
type Strobe (byteSize :: Nat) = C.BitVector byteSize

-- | The protocol does not specify the exact use of the QoS identifier. This
-- specification recommends that AxQOS is used as a priority indicator for the
-- associated write or read transaction. A higher value indicates a higher
-- priority transaction.
--
-- A default value of 0 indicates that the interface is not participating in any
-- QoS scheme.
type Qos = C.Index ((2^4) - 1)

-- | The burst type and the size information, determine how the address for
-- each transfer within the burst is calculated.
data BurstMode
  -- | In a fixed burst, the address is the same for every transfer in the
  -- burst. This burst type is used for repeated accesses to the same location
  -- such as when loading or emptying a FIFO
  = BmFixed
  -- | Incrementing. In an incrementing burst, the address for each transfer in
  -- the burst is an increment of the address for the previous transfer. The
  -- increment value depends on the size of the transfer. For example, the
  -- address for each transfer in a burst with a size of four bytes is the
  -- previous address plus four. This burst type is used for accesses to normal
  -- sequential memory.
  | BmIncr
  -- | A wrapping burst is similar to an incrementing burst, except that the
  -- address wraps around to a lower address if an upper address limit is
  -- reached. The following restrictions apply to wrapping bursts:
  --
  --   * the start address must be aligned to the size of each transfer
  --   * the length of the burst must be 2, 4, 8, or 16 transfers.
  --
  -- The behavior of a wrapping burst is:
  --
  --   * The lowest address used by the burst is aligned to the total size of
  --     the data to be transferred, that is, to ((size of each transfer in the
  --     burst) × (number of transfers in the burst)). This address is defined
  --     as the _wrap boundary_.
  --
  --   * After each transfer, the address increments in the same way as for an
  --     INCR burst. However, if this incremented address is ((wrap boundary) +
  --     (total size of data to be transferred)) then the address wraps round to
  --     the wrap boundary.
  --
  --   * The first transfer in the burst can use an address that is higher than
  --     the wrap boundary, subject to the restrictions that apply to wrapping
  --     bursts. This means that the address wraps for any WRAP burst for which
  --     the first address is higher than the wrap boundary.
  --
  -- This burst type is used for cache line accesses.
  --
  | BmWrap
  deriving (Show, C.ShowX, Generic, C.NFDataX)

-- | The maximum number of bytes to transfer in each data transfer, or beat,
-- in a burst.
data BurstSize
  = Bs1
  | Bs2
  | Bs4
  | Bs8
  | Bs16
  | Bs32
  | Bs64
  | Bs128
  deriving (Show, C.ShowX, Generic, C.NFDataX)

-- | Convert burst size to a numeric value
burstSizeToNum :: Num a => BurstSize -> a
burstSizeToNum = \case
  Bs1 -> 1
  Bs2 -> 2
  Bs4 -> 4
  Bs8 -> 8
  Bs16 -> 16
  Bs32 -> 32
  Bs64 -> 64
  Bs128 -> 128

-- | Whether a transaction is bufferable
data Bufferable = NonBufferable | Bufferable

-- | When set to "LookupCache", it is recommended that this transaction is
-- allocated in the cache for performance reasons.
data Allocate = NoLookupCache | LookupCache

-- | When set to "OtherLookupCache", it is recommended that this transaction is
-- allocated in the cache for performance reasons.
data OtherAllocate = OtherNoLookupCache | OtherLookupCache

-- | See Table A4-3 AWCACHE bit allocations
type Cache = (Bufferable, Modifiable, OtherAllocate, Allocate)

-- | Status of the write transaction.
data Resp
  -- | Normal access success. Indicates that a normal access has been
  -- successful. Can also indicate an exclusive access has failed.
  = ROkay
  -- | Exclusive access okay. Indicates that either the read or write portion
  -- of an exclusive access has been successful.
  | RExclusiveOkay
  -- | Slave error. Used when the access has reached the slave successfully, but
  -- the slave wishes to return an error condition to the originating master.
  | RSlaveError
  -- | Decode error. Generated, typically by an interconnect component, to
  -- indicate that there is no slave at the transaction address.
  | RDecodeError
  deriving (Show, C.ShowX, Generic, C.NFDataX)

-- | Whether a resource is accessed with exclusive access or not
data AtomicAccess
  = NonExclusiveAccess
  | ExclusiveAccess

-- | Whether transaction can be modified
data Modifiable
  = Modifiable
  | NonModifiable

-- | An AXI master might support Secure and Non-secure operating states, and
-- extend this concept of security to memory access.
data Secure
  = Secure
  | NonSecure

-- | An AXI master might support more than one level of operating privilege,
-- and extend this concept of privilege to memory access.
data Privileged
  = NotPrivileged
  | Privileged

-- | Whether the transaction is an instruction access or a data access. The AXI
-- protocol defines this indication as a hint. It is not accurate in all cases,
-- for example, where a transaction contains a mix of instruction and data
-- items. This specification recommends that a master sets it to "Data", to
-- indicate a data access unless the access is specifically known to be an
-- instruction access.
data InstructionOrData
  = Data
  | Instruction
