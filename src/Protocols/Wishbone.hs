{-|
Types modelling the Wishbone bus protocol.
-}

{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}

module Protocols.Wishbone where

import           Clash.Prelude         (DivRU, Nat, Type, (&&), (:::))
import qualified Clash.Prelude         as C

import           Clash.Signal.Internal (Signal (..), sample_lazy)

import           Control.DeepSeq       (NFData)
import qualified Data.Bifunctor        as B
import           Data.Data             (Proxy (Proxy))
import           Data.List             ((\\))
import           Debug.Trace
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





instance (C.NFDataX dat) => Backpressure (Wishbone dom 'Standard addressWidth dat) where
  boolsToBwd _ = C.fromList_lazy . Prelude.map (\b ->
          if b then
            wishboneS2M { acknowledge = True }
          else
            wishboneS2M { err = True }
        )


instance (C.KnownNat (C.BitSize dat), C.KnownDomain dom, C.NFDataX dat, C.KnownNat addressWidth) => Simulate (Wishbone dom 'Standard addressWidth dat) where
  type SimulateFwdType (Wishbone dom 'Standard addressWidth dat) = [WishboneM2S addressWidth (C.BitSize dat `DivRU` 8) dat]

  type SimulateBwdType (Wishbone dom 'Standard addressWidth dat) = [WishboneS2M dat]

  type SimulateChannels (Wishbone dom 'Standard addressWidth dat) = 1

  simToSigFwd Proxy = C.fromList_lazy
  simToSigBwd Proxy = C.fromList_lazy
  sigToSimFwd Proxy = sample_lazy
  sigToSimBwd Proxy = sample_lazy

  stallC config (C.head -> (stallAck, stalls)) = Circuit $
      uncurry (go stallAcks stalls (resetCycles config))
    where
      stallAcks
        | stallAck == StallCycle = [minBound..maxBound] \\ [StallCycle]
        | otherwise = [stallAck]

      toStallReply
        :: WishboneM2S addressWidth (C.BitSize dat `DivRU` 8) dat
        -> WishboneS2M dat
        -> StallAck
        -> WishboneS2M dat
      toStallReply WishboneM2S{..} s2m ack
        | strobe && busCycle = s2m
        | C.not busCycle = s2m
        | otherwise = case ack of
            StallWithNack      -> s2m { err = True }
            StallWithAck       -> s2m { acknowledge = True }
            StallWithErrorX    -> C.errorX "No defined ack"
            StallTransparently -> s2m
            StallCycle         -> s2m { acknowledge = False } -- shouldn't happen

      go
        :: [StallAck]
        -> [Int]
        -> Int
        -> Signal dom (WishboneM2S addressWidth (C.BitSize dat `DivRU` 8) dat)
        -- ^ input from master
        -> Signal dom (WishboneS2M dat)
        -- ^ reply from slave
        -> ( Signal dom (WishboneS2M dat)
           , Signal dom (WishboneM2S addressWidth (C.BitSize dat `DivRU` 8) dat))
      -- "refill" stall acks and continue
      go [] ss rs fwd bwd = go stallAcks ss rs fwd bwd

      -- resets, just pass through
      go (_:sas) _ resetN (f :- fwd) ~(b :- bwd) | resetN > 0 =
        B.bimap (b :-) (f :-) (go sas stalls (resetN - 1) fwd bwd)

      -- no more cycles to stall, just pass through
      go (s:sas) [] _ (f :- fwd) ~(b :- bwd) =
        trace ("no stalls " ++ show s ++ " : " ++ show sas) $ B.bimap (toStallReply f b s :-) (f :-) (go sas [] 0 fwd bwd)

      go (_:sas) ss _ (f :- fwd) ~(b :- bwd)
        | C.not (busCycle f) =
          -- Not in a transaction, just pass through
          trace "!CYC" $ B.bimap (wishboneS2M :-) (wishboneM2S :-) (go sas ss 0 fwd bwd)
        | busCycle f && C.not (strobe f) =
          -- LHS doesn't send any data, RHS might send a reply though, so pass it through
          trace " CYC !STB" $ B.bimap (b :-) (f :-) (go sas ss 0 fwd bwd)

      go (_sa:sas) (s:ss) _ (f0 :- fwd) ~(b0 :- bwd) =
        let
          -- Stall as long as s > 0. If s ~ 0, we wait for the RHS to acknowledge
          -- the data. As long as RHS does not acknowledge the data, we keep sending
          -- the same data.
          (f1, b1, s1) = case compare 0 s of
            LT -> (f0 { strobe = False, busCycle = True }, b0 { err = True }, pred s:ss)    -- s > 0
            EQ -> (f0, b0, if acknowledge b0 then ss else s:ss) -- s ~ 0
            GT -> error ("Unexpected negative stall: " <> show s) -- s < 0
        in
          trace "stalling" $ B.bimap (b1 :-) (f1 :-) (go sas s1 0 fwd bwd)



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
