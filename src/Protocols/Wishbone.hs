{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}

module Protocols.Wishbone where

import           Clash.Prelude

import           Protocols.Internal

-- | Data communicated from a Wishbone Master to a Wishbone Slave
data WishboneM2S bytes addressWidth
  = WishboneM2S
  { -- | ADR
    addr                :: "ADR" ::: BitVector addressWidth
    -- | DAT
  , writeData           :: "DAT_MOSI" ::: BitVector (8 * bytes)
    -- | SEL
  , busSelect           :: "SEL" ::: BitVector bytes
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
  } deriving (Generic, NFDataX, Show, Eq)


-- | Data communicated from a Wishbone Slave to a Wishbone Master
data WishboneS2M bytes
  = WishboneS2M
  { -- | DAT
    readData    :: "DAT_MISO" ::: BitVector (8 * bytes)
    -- | ACK
  , acknowledge :: "ACK"   ::: Bool
    -- | ERR
  , err         :: "ERR"   ::: Bool

    -- | STALL
  , stall       :: "STALL" ::: Bool

    -- | RTY
  , retry       :: "RTY"   ::: Bool
  } deriving (Generic, NFDataX, Show, Eq)

newtype CycleTypeIdentifier = CycleTypeIdentifier (BitVector 3) deriving (Generic, NFDataX, Show, Eq)

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
  deriving (Generic, NFDataX, Show, Eq)

data WishboneMode
  = Standard
  | Pipelined
  deriving (Generic, Show, Eq)

data Wishbone (dom :: Domain) (mode :: WishboneMode) (bytes :: Nat) (addressWidth :: Nat)

instance Protocol (Wishbone dom mode bytes addressWidth) where
  type Fwd (Wishbone dom mode bytes addressWidth) = Signal dom (WishboneM2S bytes addressWidth)
  type Bwd (Wishbone dom mode bytes addressWidth) = Signal dom (WishboneS2M bytes)


instance (KnownNat bytes) => Backpressure (Wishbone dom mode bytes addressWidth) where
  boolsToBwd _ =  fromList_lazy . Prelude.map (\b -> (wishboneS2M SNat) { acknowledge = b })


wishboneM2S :: SNat bytes -> SNat addressWidth -> WishboneM2S bytes addressWidth
wishboneM2S SNat SNat
  = WishboneM2S
  { addr = Clash.Prelude.undefined
  , writeData = Clash.Prelude.undefined
  , busSelect = Clash.Prelude.undefined
  , busCycle = False
  , strobe = False
  , writeEnable = False
  , cycleTypeIdentifier = Classic
  , burstTypeExtension = LinearBurst
  }

wishboneS2M :: SNat bytes -> WishboneS2M bytes
wishboneS2M SNat
  = WishboneS2M
  { readData = 0
  , acknowledge = False
  , err = False
  , retry = False
  , stall = False
  }
