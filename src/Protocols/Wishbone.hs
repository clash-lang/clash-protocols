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
import           Prelude               hiding (head, not, (&&))
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
instance (C.ShowX dat) => Show (WishboneS2M dat) where
  show = C.showX

newtype CycleTypeIdentifier = CycleTypeIdentifier (C.BitVector 3)
  deriving (NFData, C.Generic, C.NFDataX, Show, C.ShowX, Eq, C.BitPack)

pattern Classic, ConstantAddressBurst, IncrementingBurst, EndOfBurst :: CycleTypeIdentifier
pattern Classic = CycleTypeIdentifier 0
pattern ConstantAddressBurst = CycleTypeIdentifier 1
pattern IncrementingBurst = CycleTypeIdentifier 2
pattern EndOfBurst = CycleTypeIdentifier 7

data BurstTypeExtension
  = LinearBurst
  | Beat4Burst
  | Beat8Burst
  | Beat16Burst
  deriving (NFData, C.Generic, C.NFDataX, Show, C.ShowX, Eq, C.BitPack)

data WishboneMode
  = Standard
  | Pipelined
  deriving (C.Generic, Show, Eq)

data Wishbone (dom :: C.Domain) (mode :: WishboneMode) (addressWidth :: Nat) (userType :: Type)


instance Protocol (Wishbone dom mode addressWidth dat) where
  type Fwd (Wishbone dom mode addressWidth dat) = Signal dom (WishboneM2S addressWidth (C.BitSize dat `DivRU` 8) dat)

  type Bwd (Wishbone dom mode addressWidth dat) = Signal dom (WishboneS2M dat)





wishboneM2S :: (C.KnownNat addressWidth, C.KnownNat (C.BitSize dat), C.NFDataX dat) => WishboneM2S addressWidth (C.BitSize dat `DivRU` 8) dat
wishboneM2S
  = WishboneM2S
  { addr = C.deepErrorX "M2S address not defined"
  , writeData = C.deepErrorX "M2S writeData not defined"
  , busSelect = C.deepErrorX "M2S busSelect not defined"
  , busCycle = False
  , strobe = False
  , writeEnable = False
  , cycleTypeIdentifier = Classic
  , burstTypeExtension = LinearBurst
  }

wishboneS2M :: (C.NFDataX dat) => WishboneS2M dat
wishboneS2M
  = WishboneS2M
  { readData = C.deepErrorX "S2M readData not defined"
  , acknowledge = False
  , err = False
  , retry = False
  , stall = False
  }

terminationSignal :: WishboneS2M dat -> Bool
terminationSignal s2m = acknowledge s2m || err s2m || retry s2m
