{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

{- |
Types modelling the Wishbone bus protocol.
-}
module Protocols.Wishbone where

import Clash.Prelude (DivRU, Nat, Type, (:::))
import Prelude hiding (head, not, (&&))

import Clash.Signal.Internal (Signal (..))
import Control.DeepSeq (NFData)
import Protocols
import Protocols.Idle

import qualified Clash.Prelude as C

-- | Data communicated from a Wishbone Master to a Wishbone Slave
data WishboneM2S addressWidth selWidth dat = WishboneM2S
  { addr :: "ADR" ::: C.BitVector addressWidth
  -- ^ The address output array [ADR_O()] is used to pass a binary address. The higher array
  --   boundary is specific to the address width of the core, and the lower array boundary is
  --   determined by the data port size and granularity. For example the array size on a 32-bit
  --   data port with BYTE granularity is [ADR_O(n..2)]. In some cases (such as FIFO
  --   interfaces) the array may not be present on the interface.
  , writeData :: "DAT_MOSI" ::: dat
  -- ^ The data output array [DAT_O()] is used to pass binary data. The array boundaries are
  --   determined by the port size, with a maximum port size of 64-bits (e.g. [DAT_I(63..0)]).
  , busSelect :: "SEL" ::: C.BitVector selWidth
  -- ^ The select output array [SEL_O()] indicates where valid data is expected on the [DAT_I()]
  --   signal array during READ cycles, and where it is placed on the [DAT_O()] signal array
  --   during WRITE cycles. The array boundaries are determined by the granularity of a port.
  --   For example, if 8-bit granularity is used on a 64-bit port, then there would be an array of
  --   eight select signals with boundaries of [SEL_O(7..0)]. Each individual select signal
  --   correlates to one of eight active bytes on the 64-bit data port.
  , lock :: "LOCK" ::: Bool
  -- ^ The lock output [LOCK_O] when asserted, indicates that the current bus cycle is
  --   uninterruptible. Lock is asserted to request complete ownership of the bus. Once the
  --   transfer has started, the INTERCON does not grant the bus to any other MASTER, until
  --   the current MASTER negates [LOCK_O] or [CYC_O].
  , busCycle :: "CYC" ::: Bool
  -- ^ The cycle output [CYC_O], when asserted, indicates that a valid bus cycle is in progress.
  --   The signal is asserted for the duration of all bus cycles. For example, during a BLOCK
  --   transfer cycle there can be multiple data transfers. The [CYC_O] signal is asserted during
  --   the first data transfer, and remains asserted until the last data transfer. The [CYC_O]
  --   signal is useful for interfaces with multi-port interfaces (such as dual port memories). In
  --   these cases, the [CYC_O] signal requests use of a common bus from an arbiter.
  , strobe :: "STB" ::: Bool
  -- ^ The strobe output [STB_O] indicates a valid data transfer cycle. It is used to qualify
  --   various other signals on the interface such as [SEL_O()]. The SLAVE asserts either the
  --   [ACK_I], [ERR_I] or [RTY_I] signals in response to every assertion of the [STB_O] signal.
  , writeEnable :: "WE" ::: Bool
  -- ^ The write enable output [WE_O] indicates whether the current local bus cycle is a READ
  --   or WRITE cycle. The signal is negated during READ cycles, and is asserted during WRITE
  --   cycles.
  , cycleTypeIdentifier :: "CTI" ::: CycleTypeIdentifier
  -- ^ The Cycle Type Idenfier [CTI_IO()] Address Tag provides additional information
  --   about the current cycle. The MASTER sends this information to the SLAVE. The SLAVE
  --   can use this information to prepare the response for the next cycle.
  , burstTypeExtension :: "BTE" ::: BurstTypeExtension
  -- ^ The Burst Type Extension [BTE_O()] Address Tag is send by the MASTER to the
  --   SLAVE to provides additional information about the current burst. Currently this
  --   information is only relevant for incrementing bursts, but future burst types may use these
  --   signals.
  }
  deriving (NFData, C.Generic, C.NFDataX, Eq, C.BitPack)

instance
  (C.ShowX dat, C.KnownNat addressWidth, C.KnownNat selWidth) =>
  C.ShowX (WishboneM2S addressWidth selWidth dat)
  where
  showX = show

-- Compact printing for M2S values. This handles undefined values in the
-- structure too.
instance
  (C.ShowX dat, C.KnownNat addressWidth, C.KnownNat selWidth) =>
  Show (WishboneM2S addressWidth selWidth dat)
  where
  show WishboneM2S{..} =
    "WishboneM2S [ "
      <> prefix busCycle
      <> "CYC "
      <> prefix strobe
      <> "STB "
      <> prefix writeEnable
      <> "WE, "
      <> "ADR = "
      <> C.showX addr
      <> ", "
      <> "DAT = "
      <> C.showX writeData
      <> ", "
      <> "SEL = "
      <> C.showX busSelect
      <> ", "
      <> "CTE = "
      <> cycle'
      <> ", "
      <> "BTE = "
      <> burst
      <> " ]"
   where
    prefix True = " "
    prefix False = "!"

    burst = case burstTypeExtension of
      LinearBurst -> "linear"
      Beat4Burst -> "beat 4"
      Beat8Burst -> "beat 8"
      Beat16Burst -> "beat 16"

    cycle' = case cycleTypeIdentifier of
      Classic -> "classic"
      ConstantAddressBurst -> "constant addr"
      IncrementingBurst -> "incrementing"
      EndOfBurst -> "end-of-burst"
      CycleTypeIdentifier val -> "reserved (" <> C.showX val <> ")"

-- | Data communicated from a Wishbone Slave to a Wishbone Master
data WishboneS2M dat = WishboneS2M
  { readData :: "DAT_MISO" ::: dat
  -- ^ The data output array [DAT_O()] is used to pass binary data. The array boundaries are
  --   determined by the port size, with a maximum port size of 64-bits (e.g. [DAT_I(63..0)]).
  , acknowledge :: "ACK" ::: Bool
  -- ^ The acknowledge output [ACK_O], when asserted, indicates the termination of a normal
  --   bus cycle. Also see the [ERR_O] and [RTY_O] signal descriptions.
  , err :: "ERR" ::: Bool
  -- ^ The error output [ERR_O] indicates an abnormal cycle termination. The source of the
  --   error, and the response generated by the MASTER is defined by the IP core supplier. Also
  --   see the [ACK_O] and [RTY_O] signal descriptions.
  , stall :: "STALL" ::: Bool
  -- ^ The pipeline stall signal [STALL_O] indicates that the slave can not accept additional
  --   transactions in its queue.
  --   This signal is used in pipelined mode
  , retry :: "RTY" ::: Bool
  -- ^ The retry output [RTY_O] indicates that the indicates that the interface is not ready to
  --   accept or send data, and that the cycle should be retried. When and how the cycle is retried
  --   is defined by the IP core supplier. Also see the [ERR_O] and [RTY_O] signal descriptions.
  }
  deriving (NFData, C.Generic, C.NFDataX, Eq, C.BitPack)

instance (C.ShowX dat) => C.ShowX (WishboneS2M dat) where
  showX = show

-- Compact printing for S2M values. This handles undefined values in the
-- structure too.
instance (C.ShowX dat) => Show (WishboneS2M dat) where
  show WishboneS2M{..} =
    "WishboneS2M [ "
      <> prefix acknowledge
      <> "ACK "
      <> prefix err
      <> "ERR "
      <> prefix stall
      <> "STALL "
      <> prefix stall
      <> "RETRY, "
      <> "DAT = "
      <> C.showX readData
      <> " ]"
   where
    prefix True = " "
    prefix False = "!"

{- | Identifier for different types of cycle-modes, used to potentially
  increase throughput by reducing handshake-overhead
-}
newtype CycleTypeIdentifier = CycleTypeIdentifier (C.BitVector 3)
  deriving stock (Eq, Show, C.Generic)
  deriving anyclass (NFData, C.NFDataX, C.ShowX, C.BitPack)

pattern
  Classic
  , ConstantAddressBurst
  , IncrementingBurst
  , EndOfBurst ::
    CycleTypeIdentifier

-- | Classic Wishbone cycle type
pattern Classic = CycleTypeIdentifier 0

-- | Burst Wishbone cycle using a constant address (and operation)
pattern ConstantAddressBurst = CycleTypeIdentifier 1

-- | Burst incrementing the address based on burst-types
pattern IncrementingBurst = CycleTypeIdentifier 2

-- | Cycle-Type-Identifier signalling the end of a non-classic cycle
pattern EndOfBurst = CycleTypeIdentifier 7

-- | Burst-mode types when 'IncrementingBurst' cycle type is used
data BurstTypeExtension
  = -- | Linear address-increase
    LinearBurst
  | -- | Wrap-4, address LSBs are modulo 4
    Beat4Burst
  | -- | Wrap-8, address LSBs are modulo 8
    Beat8Burst
  | -- | Wrap-16, address LSBs are modulo 16
    Beat16Burst
  deriving (NFData, C.Generic, C.NFDataX, Show, C.ShowX, Eq, C.BitPack)

-- | Wishbone protocol mode that a component operates in
data WishboneMode
  = -- | Standard mode, generally using a "wait-for-ack" approach
    Standard
  | -- | Pipelines mode, generally allowing for more asynchronous requests
    Pipelined
  deriving (C.Generic, Show, Eq)

-- | The Wishbone protocol (http://cdn.opencores.org/downloads/wbspec_b4.pdf)
data
  Wishbone
    (dom :: C.Domain)
    (mode :: WishboneMode)
    (addressWidth :: Nat)
    (userType :: Type)

instance Protocol (Wishbone dom mode addressWidth dat) where
  type
    Fwd (Wishbone dom mode addressWidth dat) =
      Signal dom (WishboneM2S addressWidth (C.BitSize dat `DivRU` 8) dat)

  type Bwd (Wishbone dom mode addressWidth dat) = Signal dom (WishboneS2M dat)

instance
  (C.KnownNat aw, C.KnownNat (C.BitSize dat), C.NFDataX dat) =>
  IdleCircuit (Wishbone dom mode aw dat)
  where
  idleFwd _ = C.pure emptyWishboneM2S
  idleBwd _ = C.pure emptyWishboneS2M

-- | Construct "default" Wishbone M2S signals
emptyWishboneM2S ::
  (C.KnownNat addressWidth, C.KnownNat (C.BitSize dat), C.NFDataX dat) =>
  WishboneM2S addressWidth (C.BitSize dat `DivRU` 8) dat
emptyWishboneM2S =
  WishboneM2S
    { addr = C.deepErrorX "M2S address not defined"
    , writeData = C.deepErrorX "M2S writeData not defined"
    , busSelect = C.deepErrorX "M2S busSelect not defined"
    , lock = False
    , busCycle = False
    , strobe = False
    , writeEnable = False
    , cycleTypeIdentifier = Classic
    , burstTypeExtension = LinearBurst
    }

-- | Construct "default" Wishbone S2M signals
emptyWishboneS2M :: (C.NFDataX dat) => WishboneS2M dat
emptyWishboneS2M =
  WishboneS2M
    { readData = C.deepErrorX "S2M readData not defined"
    , acknowledge = False
    , err = False
    , retry = False
    , stall = False
    }

-- | Helper function to determine whether a Slave signals the termination of a cycle.
hasTerminateFlag :: WishboneS2M dat -> Bool
hasTerminateFlag s2m = acknowledge s2m || err s2m || retry s2m

{- | Force a /nack/ on the backward channel and /no data/ on the forward
channel if reset is asserted.
-}
forceResetSanity ::
  forall dom mode aw a.
  ( C.HiddenClockResetEnable dom
  , C.KnownNat aw
  , C.KnownNat (C.BitSize a)
  , C.NFDataX a
  ) =>
  Circuit (Wishbone dom mode aw a) (Wishbone dom mode aw a)
forceResetSanity = forceResetSanityGeneric
