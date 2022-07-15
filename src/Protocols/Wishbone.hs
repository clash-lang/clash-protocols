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
  { -- | ADR
    addr                :: "ADR" ::: C.BitVector addressWidth
    -- | DAT
  , writeData           :: "DAT_MOSI" ::: dat
    -- | SEL
  , busSelect           :: "SEL" ::: C.BitVector selWidth
    -- | LOCK
  , lock                :: "LOCK" ::: Bool
    -- | CYC
  , busCycle            :: "CYC" ::: Bool
    -- | STB
  , strobe              :: "STB" ::: Bool
    -- | WE
  , writeEnable         :: "WE" ::: Bool
    -- | CTI
  , cycleTypeIdentifier :: "CTI" ::: CycleTypeIdentifier
    -- | BTE
  , burstTypeExtension  :: "BTE" ::: BurstTypeExtension
  } deriving (NFData, C.Generic, C.NFDataX, C.ShowX, Eq, C.BitPack)

-- M2S signals can contain undefined values, hence 'Show' is implemented through 'ShowX'
instance (C.ShowX dat, C.KnownNat addressWidth, C.KnownNat selWidth) => Show (WishboneM2S addressWidth selWidth dat) where
  show = C.showX

-- | Data communicated from a Wishbone Slave to a Wishbone Master
data WishboneS2M dat
  = WishboneS2M
  { -- | DAT
    readData    :: "DAT_MISO" ::: dat
    -- | ACK
  , acknowledge :: "ACK"   ::: Bool
    -- | ERR
  , err         :: "ERR"   ::: Bool

    -- | STALL
  , stall       :: "STALL" ::: Bool

    -- | RTY
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
wishboneM2S :: (C.KnownNat addressWidth, C.KnownNat (C.BitSize dat), C.NFDataX dat) => WishboneM2S addressWidth (C.BitSize dat `DivRU` 8) dat
wishboneM2S
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
wishboneS2M :: (C.NFDataX dat) => WishboneS2M dat
wishboneS2M
  = WishboneS2M
  { readData = C.deepErrorX "S2M readData not defined"
  , acknowledge = False
  , err = False
  , retry = False
  , stall = False
  }

-- | Helper function to determine whether a Slave signals the termination of a cycle.
terminationSignal :: WishboneS2M dat -> Bool
terminationSignal s2m = acknowledge s2m || err s2m || retry s2m


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

        m2ss' = C.liftA3 C.replace sIdx m2s $ pure (C.repeat wishboneM2S)
        s2ms' = C.liftA3 C.replace mIdx s2m $ pure (C.repeat wishboneS2M)


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
        m2ss' = C.scatter @_ @_ @_ @_ @0 (C.repeat wishboneM2S) <$> route <*> m2ss
        s2ms' = C.gather <$> s2ms <*> route
