{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
-- NFDataX and ShowX for T3 and T4
{-# OPTIONS_GHC -fno-warn-orphans #-}

{- |
Types and utilities shared between AXI4, AXI4-Lite, and AXI3.
-}
module Protocols.Axi4.Common where

-- base

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import GHC.TypeNats (Nat)

-- clash-prelude

import Clash.Prelude (type (-), type (^))
import qualified Clash.Prelude as C

-- strict-tuple
import Data.Tuple.Strict (T3, T4)

import Protocols.Internal

deriving instance
  ( C.NFDataX a
  , C.NFDataX b
  , C.NFDataX c
  ) =>
  C.NFDataX (T3 a b c)

deriving instance
  ( C.NFDataX a
  , C.NFDataX b
  , C.NFDataX c
  , C.NFDataX d
  ) =>
  C.NFDataX (T4 a b c d)

deriving instance
  ( C.ShowX a
  , C.ShowX b
  , C.ShowX c
  ) =>
  C.ShowX (T3 a b c)

deriving instance
  ( C.ShowX a
  , C.ShowX b
  , C.ShowX c
  , C.ShowX d
  ) =>
  C.ShowX (T4 a b c d)

-- | Enables or disables 'BurstMode'
type BurstType (keep :: Bool) = KeepType keep BurstMode

-- | Enables or disables burst length
type BurstLengthType (keep :: Bool) = KeepType keep (C.Index (2 ^ 8))

-- | Enables or disables 'Cache'
type CacheType (keep :: Bool) = KeepType keep Cache

-- | Enables or disables a boolean indicating whether a transaction is done
type LastType (keep :: Bool) = KeepType keep Bool

-- | Enables or disables 'AtomicAccess'
type LockType (keep :: Bool) = KeepType keep AtomicAccess

-- | Enables or disables 'Privileged', 'Secure', and 'InstructionOrData'
type PermissionsType (keep :: Bool) =
  KeepType keep (T3 Privileged Secure InstructionOrData)

-- | Enables or disables 'Qos'
type QosType (keep :: Bool) = KeepType keep Qos

-- | Enables or disables region type
type RegionType (keep :: Bool) = KeepType keep (C.BitVector 4)

-- | Enables or disables 'Resp'
type ResponseType (keep :: Bool) = KeepType keep Resp

-- | Enables or disables 'BurstSize'
type SizeType (keep :: Bool) = KeepType keep BurstSize

{- | @byteSize@ bytes of data,
with @keepStrobe@ determining whether to include a strobe value or not.
-}
type StrictStrobeType (byteSize :: Nat) (keepStrobe :: Bool) =
  C.Vec byteSize (StrobeDataType keepStrobe)

-- | Enable or disable a strobe value.
type family StrobeDataType (keepStrobe :: Bool) = t | t -> keepStrobe where
  StrobeDataType 'True = Maybe (C.BitVector 8)
  StrobeDataType 'False = C.BitVector 8

{- | We want to define operations on 'StrobeDataType' that work for both possibilities
(@keepStrobe = 'True@ and @keepStrobe = 'False@), but we can't pattern match directly.
Instead we need to define a class and instantiate
the class for both @'True@ and @'False@.
-}
class KeepStrobeClass (keepStrobe :: Bool) where
  -- | Get the value of @keepStrobe@ at the term level.
  getKeepStrobe :: StrobeDataType keepStrobe -> Bool

  -- | Convert a byte into a possibly-strobed byte.
  -- The 'Bool' value determines the strobe value
  -- if strobe is enabled.
  toStrobeDataType :: Bool -> C.BitVector 8 -> StrobeDataType keepStrobe

  -- | Convert a possibly-strobed byte into a byte,
  -- or 'Nothing' if strobe is enabled and strobe = false.
  fromStrobeDataType :: StrobeDataType keepStrobe -> Maybe (C.BitVector 8)

instance KeepStrobeClass 'True where
  getKeepStrobe _ = True
  toStrobeDataType True d = Just d
  toStrobeDataType False _ = Nothing
  fromStrobeDataType v = v

instance KeepStrobeClass 'False where
  getKeepStrobe _ = False
  toStrobeDataType _ d = d
  fromStrobeDataType v = Just v

{- | The protocol does not specify the exact use of the QoS identifier. This
specification recommends that AxQOS is used as a priority indicator for the
associated write or read transaction. A higher value indicates a higher
priority transaction.

A default value of 0 indicates that the interface is not participating in any
QoS scheme.
-}
type Qos = C.Index ((2 ^ 4) - 1)

{- | The burst type and the size information, determine how the address for
each transfer within the burst is calculated.
-}
data BurstMode
  = -- | In a fixed burst, the address is the same for every transfer in the
    -- burst. This burst type is used for repeated accesses to the same location
    -- such as when loading or emptying a FIFO
    BmFixed
  | -- | Incrementing. In an incrementing burst, the address for each transfer in
    -- the burst is an increment of the address for the previous transfer. The
    -- increment value depends on the size of the transfer. For example, the
    -- address for each transfer in a burst with a size of four bytes is the
    -- previous address plus four. This burst type is used for accesses to normal
    -- sequential memory.
    BmIncr
  | -- | A wrapping burst is similar to an incrementing burst, except that the
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
    BmWrap
  deriving (Show, C.ShowX, Generic, C.NFDataX, NFData, Eq)

{- | The maximum number of bytes to transfer in each data transfer, or beat,
in a burst.
-}
data BurstSize
  = Bs1
  | Bs2
  | Bs4
  | Bs8
  | Bs16
  | Bs32
  | Bs64
  | Bs128
  deriving (Show, C.ShowX, Generic, C.NFDataX, NFData, Eq)

-- | Convert burst size to a numeric value
burstSizeToNum :: (Num a) => BurstSize -> a
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
  deriving (Show, C.ShowX, Generic, C.NFDataX, NFData, Eq)

{- | When set to "LookupCache", it is recommended that this transaction is
allocated in the cache for performance reasons.
-}
data Allocate = NoLookupCache | LookupCache
  deriving (Show, C.ShowX, Generic, C.NFDataX, NFData, Eq)

{- | When set to "OtherLookupCache", it is recommended that this transaction is
allocated in the cache for performance reasons.
-}
data OtherAllocate = OtherNoLookupCache | OtherLookupCache
  deriving (Show, C.ShowX, Generic, C.NFDataX, NFData, Eq)

-- | See Table A4-3 AWCACHE bit allocations
type Cache = T4 Bufferable Modifiable OtherAllocate Allocate

-- | Status of the write transaction.
data Resp
  = -- | Normal access success. Indicates that a normal access has been
    -- successful. Can also indicate an exclusive access has failed.
    ROkay
  | -- | Exclusive access okay. Indicates that either the read or write portion
    -- of an exclusive access has been successful.
    RExclusiveOkay
  | -- | Slave error. Used when the access has reached the slave successfully, but
    -- the slave wishes to return an error condition to the originating master.
    RSlaveError
  | -- | Decode error. Generated, typically by an interconnect component, to
    -- indicate that there is no slave at the transaction address.
    RDecodeError
  deriving (Show, C.ShowX, Generic, C.NFDataX, NFData, Eq)

-- | Whether a resource is accessed with exclusive access or not
data AtomicAccess
  = NonExclusiveAccess
  | ExclusiveAccess
  deriving (Show, C.ShowX, Generic, C.NFDataX, NFData, Eq)

-- | Whether transaction can be modified
data Modifiable
  = Modifiable
  | NonModifiable
  deriving (Show, C.ShowX, Generic, C.NFDataX, NFData, Eq)

{- | An AXI master might support Secure and Non-secure operating states, and
extend this concept of security to memory access.
-}
data Secure
  = Secure
  | NonSecure
  deriving (Show, C.ShowX, Generic, C.NFDataX, NFData, Eq)

{- | An AXI master might support more than one level of operating privilege,
and extend this concept of privilege to memory access.
-}
data Privileged
  = NotPrivileged
  | Privileged
  deriving (Show, C.ShowX, Generic, C.NFDataX, NFData, Eq)

{- | Whether the transaction is an instruction access or a data access. The AXI
protocol defines this indication as a hint. It is not accurate in all cases,
for example, where a transaction contains a mix of instruction and data
items. This specification recommends that a master sets it to "Data", to
indicate a data access unless the access is specifically known to be an
instruction access.
-}
data InstructionOrData
  = Data
  | Instruction
  deriving (Show, C.ShowX, Generic, C.NFDataX, NFData, Eq)
