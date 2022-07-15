{-|
Types modelling the Wishbone bus protocol.
-}

{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}

module Protocols.Wishbone where

import           Clash.Prelude         (DivRU, Nat, Type, (:::))
import qualified Clash.Prelude         as C

import           Clash.Signal.Internal (Signal (..))

import           Control.DeepSeq       (NFData)
import qualified Data.Bifunctor        as B
import           Prelude               hiding (head, not, (&&))
import           Protocols
import           Protocols.Internal

-- | Data communicated from a Wishbone Master to a Wishbone Slave
data WishboneM2S addressWidth selWidth dat
  = WishboneM2S
  { -- | The address output array [ADR_O()] is used to pass a binary address. The higher array
    --   boundary is specific to the address width of the core, and the lower array boundary is
    --   determined by the data port size and granularity. For example the array size on a 32-bit
    --   data port with BYTE granularity is [ADR_O(n..2)]. In some cases (such as FIFO
    --   interfaces) the array may not be present on the interface.
    addr                :: "ADR" ::: C.BitVector addressWidth
    -- | The data output array [DAT_O()] is used to pass binary data. The array boundaries are
    --   determined by the port size, with a maximum port size of 64-bits (e.g. [DAT_I(63..0)]).
  , writeData           :: "DAT_MOSI" ::: dat
    -- | The select output array [SEL_O()] indicates where valid data is expected on the [DAT_I()]
    --   signal array during READ cycles, and where it is placed on the [DAT_O()] signal array
    --   during WRITE cycles. The array boundaries are determined by the granularity of a port.
    --   For example, if 8-bit granularity is used on a 64-bit port, then there would be an array of
    --   eight select signals with boundaries of [SEL_O(7..0)]. Each individual select signal
    --   correlates to one of eight active bytes on the 64-bit data port.
  , busSelect           :: "SEL" ::: C.BitVector selWidth
    -- | The lock output [LOCK_O] when asserted, indicates that the current bus cycle is
    --   uninterruptible. Lock is asserted to request complete ownership of the bus. Once the
    --   transfer has started, the INTERCON does not grant the bus to any other MASTER, until
    --   the current MASTER negates [LOCK_O] or [CYC_O].
  , lock                :: "LOCK" ::: Bool
    -- | The cycle output [CYC_O], when asserted, indicates that a valid bus cycle is in progress.
    --   The signal is asserted for the duration of all bus cycles. For example, during a BLOCK
    --   transfer cycle there can be multiple data transfers. The [CYC_O] signal is asserted during
    --   the first data transfer, and remains asserted until the last data transfer. The [CYC_O]
    --   signal is useful for interfaces with multi-port interfaces (such as dual port memories). In
    --   these cases, the [CYC_O] signal requests use of a common bus from an arbiter.
  , busCycle            :: "CYC" ::: Bool
    -- | The strobe output [STB_O] indicates a valid data transfer cycle. It is used to qualify
    --   various other signals on the interface such as [SEL_O()]. The SLAVE asserts either the
    --   [ACK_I], [ERR_I] or [RTY_I] signals in response to every assertion of the [STB_O] signal.
  , strobe              :: "STB" ::: Bool
    -- | The write enable output [WE_O] indicates whether the current local bus cycle is a READ
    --   or WRITE cycle. The signal is negated during READ cycles, and is asserted during WRITE
    --   cycles.
  , writeEnable         :: "WE" ::: Bool
    -- | The Cycle Type Idenfier [CTI_IO()] Address Tag provides additional information
    --   about the current cycle. The MASTER sends this information to the SLAVE. The SLAVE
    --   can use this information to prepare the response for the next cycle.
  , cycleTypeIdentifier :: "CTI" ::: CycleTypeIdentifier
    -- | The Burst Type Extension [BTE_O()] Address Tag is send by the MASTER to the
    --   SLAVE to provides additional information about the current burst. Currently this
    --   information is only relevant for incrementing bursts, but future burst types may use these
    --   signals.
  , burstTypeExtension  :: "BTE" ::: BurstTypeExtension
  } deriving (NFData, C.Generic, C.NFDataX, C.ShowX, Eq, C.BitPack)

-- M2S signals can contain undefined values, hence 'Show' is implemented through 'ShowX'
instance (C.ShowX dat, C.KnownNat addressWidth, C.KnownNat selWidth) => Show (WishboneM2S addressWidth selWidth dat) where
  show = C.showX

-- | Data communicated from a Wishbone Slave to a Wishbone Master
data WishboneS2M dat
  = WishboneS2M
  { -- | The data output array [DAT_O()] is used to pass binary data. The array boundaries are
    --   determined by the port size, with a maximum port size of 64-bits (e.g. [DAT_I(63..0)]).
    readData    :: "DAT_MISO" ::: dat
    -- | The acknowledge output [ACK_O], when asserted, indicates the termination of a normal
    --   bus cycle. Also see the [ERR_O] and [RTY_O] signal descriptions.
  , acknowledge :: "ACK"   ::: Bool
    -- | The error output [ERR_O] indicates an abnormal cycle termination. The source of the
    --   error, and the response generated by the MASTER is defined by the IP core supplier. Also
    --   see the [ACK_O] and [RTY_O] signal descriptions.
  , err         :: "ERR"   ::: Bool
    -- | The pipeline stall signal [STALL_O] indicates that the slave can not accept additional
    --   transactions in its queue.
    --   This signal is used in pipelined mode
  , stall       :: "STALL" ::: Bool
    -- | The retry output [RTY_O] indicates that the indicates that the interface is not ready to
    --   accept or send data, and that the cycle should be retried. When and how the cycle is retried
    --   is defined by the IP core supplier. Also see the [ERR_O] and [RTY_O] signal descriptions.
  , retry       :: "RTY"   ::: Bool
  } deriving (NFData, C.Generic, C.NFDataX, C.ShowX, Eq, C.BitPack)

-- S2M signals can contain undefined values, hence 'Show' is implemented through 'ShowX'
instance (C.ShowX dat) => Show (WishboneS2M dat) where
  show = C.showX

-- | Identifier for different types of cycle-modes, used to potentially
--   increase throughput by reducing handshake-overhead
newtype CycleTypeIdentifier = CycleTypeIdentifier (C.BitVector 3)
  deriving (NFData, C.Generic, C.NFDataX, Show, C.ShowX, Eq, C.BitPack)

pattern Classic, ConstantAddressBurst, IncrementingBurst, EndOfBurst :: CycleTypeIdentifier
-- ^ Classic Wishbone cycle type
pattern Classic = CycleTypeIdentifier 0
-- ^ Burst Wishbone cycle using a constant address (and operation)
pattern ConstantAddressBurst = CycleTypeIdentifier 1
-- ^ Burst incrementing the address based on burst-types
pattern IncrementingBurst = CycleTypeIdentifier 2
-- ^ Cycle-Type-Identifier signalling the end of a non-classic cycle
pattern EndOfBurst = CycleTypeIdentifier 7

-- | Burst-mode types when 'IncrementingBurst' cycle type is used
data BurstTypeExtension
  -- | Linear address-increase
  = LinearBurst
  -- | Wrap-4, address LSBs are modulo 4
  | Beat4Burst
  -- | Wrap-8, address LSBs are modulo 8
  | Beat8Burst
  -- | Wrap-16, address LSBs are modulo 16
  | Beat16Burst
  deriving (NFData, C.Generic, C.NFDataX, Show, C.ShowX, Eq, C.BitPack)

-- | Wishbone protocol mode that a component operates in
data WishboneMode
  -- | Standard mode, generally using a "wait-for-ack" approach
  = Standard
  -- | Pipelines mode, generally allowing for more asynchronous requests
  | Pipelined
  deriving (C.Generic, Show, Eq)

-- | The Wishbone protocol (http://cdn.opencores.org/downloads/wbspec_b4.pdf)
data Wishbone (dom :: C.Domain) (mode :: WishboneMode) (addressWidth :: Nat) (userType :: Type)


instance Protocol (Wishbone dom mode addressWidth dat) where
  type Fwd (Wishbone dom mode addressWidth dat) = Signal dom (WishboneM2S addressWidth (C.BitSize dat `DivRU` 8) dat)

  type Bwd (Wishbone dom mode addressWidth dat) = Signal dom (WishboneS2M dat)


-- | Construct "default" Wishbone M2S signals
emptyWishboneM2S :: (C.KnownNat addressWidth, C.KnownNat (C.BitSize dat), C.NFDataX dat) => WishboneM2S addressWidth (C.BitSize dat `DivRU` 8) dat
emptyWishboneM2S
  = WishboneM2S
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
emptyWishboneS2M
  = WishboneS2M
  { readData = C.deepErrorX "S2M readData not defined"
  , acknowledge = False
  , err = False
  , retry = False
  , stall = False
  }

-- | Helper function to determine whether a Slave signals the termination of a cycle.
hasTerminateFlag :: WishboneS2M dat -> Bool
hasTerminateFlag s2m = acknowledge s2m || err s2m || retry s2m


-- | Distribute requests amongst N slave circuits
roundrobin
  :: forall n dom addressWidth a
  . (C.KnownNat n, C.HiddenClockResetEnable dom, C.KnownNat addressWidth, C.KnownNat (C.BitSize a), C.NFDataX a, 1 C.<= n)
  => Circuit (Wishbone dom 'Standard addressWidth a) (C.Vec n (Wishbone dom 'Standard addressWidth a))
roundrobin = Circuit $ \(m2s, s2ms) -> B.first C.head $ fn (C.singleton m2s, s2ms)
  where
    Circuit fn = sharedBus selectFn
    selectFn (C.unbundle -> (mIdx, sIdx, _)) = C.liftA2 (,) mIdx (C.satSucc C.SatWrap <$> sIdx)

-- | General-purpose shared-bus with N masters and M slaves.
--
--   A selector signal is used to compute the next M-S pair.
sharedBus
  :: forall n m dom addressWidth a
  . (C.KnownNat n, C.KnownNat m, C.HiddenClockResetEnable dom, C.KnownNat addressWidth, C.KnownNat (C.BitSize a), C.NFDataX a)
  => (C.Signal dom (C.Index n, C.Index m, C.Vec n (WishboneM2S addressWidth (C.BitSize a `DivRU` 8) a)) -> C.Signal dom (C.Index n, C.Index m))
  -- ^ Funcion to select which M-S pair should be connected next.
  -> Circuit
      (C.Vec n (Wishbone dom 'Standard addressWidth a))
      (C.Vec m (Wishbone dom 'Standard addressWidth a))
sharedBus selectFn = Circuit go
  where
    go :: (C.Vec n (Signal dom (WishboneM2S addressWidth (C.Div (C.BitSize a C.+ 7) 8) a)), C.Vec m (Signal dom (WishboneS2M a)))
       -> (C.Vec n (Signal dom (WishboneS2M a)), C.Vec m (Signal dom (WishboneM2S addressWidth (C.Div (C.BitSize a C.+ 7) 8) a)))
    go (C.bundle -> m2ss, C.bundle -> s2ms) = (C.unbundle s2ms', C.unbundle m2ss')
      where
        mIdx = C.regEn (0 :: C.Index n) acceptIdx mIdx'
        sIdx = C.regEn (0 :: C.Index m) acceptIdx sIdx'

        (mIdx', sIdx') = C.unbundle $ selectFn (C.liftA3 (,,) mIdx sIdx m2ss)

        m2s = C.liftA2 (C.!!) m2ss mIdx
        s2m = C.liftA2 (C.!!) s2ms sIdx

        acceptIdx = (C.not . busCycle <$> m2s) C..&&. (C.not . lock <$> m2s)

        m2ss' = C.liftA3 C.replace sIdx m2s $ pure (C.repeat emptyWishboneM2S)
        s2ms' = C.liftA3 C.replace mIdx s2m $ pure (C.repeat emptyWishboneS2M)


-- | Crossbar-Switch circuit, allowing to dynamically route N masters to N slaves
crossbarSwitch
  :: forall n m dom addressWidth a
  . (C.KnownNat n, C.KnownNat m, C.KnownDomain dom, C.KnownNat addressWidth, C.NFDataX a, C.KnownNat (C.BitSize a))
  => Circuit
    -- route                              masters
      (CSignal dom (C.Vec n (C.Index m)), C.Vec n (Wishbone dom 'Standard addressWidth a))
    -- slaves
      (C.Vec m (Wishbone dom 'Standard addressWidth a))
crossbarSwitch = Circuit go
  where
    go ((CSignal route, C.bundle -> m2ss), C.bundle -> s2ms) = ((CSignal (pure ()), C.unbundle s2ms'), C.unbundle m2ss')
      where
        m2ss' = C.scatter @_ @_ @_ @_ @0 (C.repeat emptyWishboneM2S) <$> route <*> m2ss
        s2ms' = C.gather <$> s2ms <*> route
