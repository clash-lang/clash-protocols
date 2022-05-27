{-|
Types modelling the Wishbone bus protocol.
-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE UndecidableInstances  #-}

module Protocols.Wishbone where

import           Clash.Prelude              hiding (length, sample_lazy, (||))
import           Clash.Signal.Internal      (Signal (..), sample_lazy, Reset (..))

import           Control.DeepSeq            (NFData)
import qualified Data.Bifunctor             as B
import           Data.Data                  (Proxy (Proxy))
import           Data.List                  ((\\))
import           Data.Maybe                 (catMaybes, fromMaybe, mapMaybe)
import           GHC.Stack                  (HasCallStack)
import           Hedgehog                   (MonadTest)
import           Prelude                    hiding (head, not, (&&))
import           Protocols.Hedgehog         (ExpectOptions (..), Test (..))
import           Protocols.Internal
import           Protocols.Df               (Data(..))

-- hedgehog
import qualified Hedgehog                   as H
import qualified Hedgehog.Internal.Property as H
import qualified Hedgehog.Internal.Show     as H
import           Text.Show.Pretty           (ppShow)

import qualified Clash.Explicit.Prelude     as E
import           Control.Monad.State        (get, gets, modify, put, runState)
import           Control.Monad              (when)
import           Control.Arrow              (first, second)

-- | Data communicated from a Wishbone Master to a Wishbone Slave
data WishboneM2S addressWidth selWidth dat
  = WishboneM2S
  { -- | ADR
    addr                :: "ADR" ::: BitVector addressWidth
    -- | DAT
  , writeData           :: "DAT_MOSI" ::: dat
    -- | SEL
  , busSelect           :: "SEL" ::: BitVector selWidth
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
  } deriving (NFData, Generic, NFDataX, Show, ShowX, Eq)


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
  } deriving (NFData, Generic, NFDataX, Show, ShowX, Eq)

newtype CycleTypeIdentifier = CycleTypeIdentifier (BitVector 3) deriving (NFData, Generic, NFDataX, Show, ShowX, Eq)

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
  deriving (NFData, Generic, NFDataX, Show, ShowX, Eq)

data WishboneMode
  = Standard
  | Pipelined
  deriving (Generic, Show, Eq)

data Wishbone (dom :: Domain) (mode :: WishboneMode) (addressWidth :: Nat) (userType :: Type)


instance Protocol (Wishbone dom mode addressWidth dat) where
  type Fwd (Wishbone dom mode addressWidth dat) = Signal dom (WishboneM2S addressWidth (BitSize dat `DivRU` 8) dat)

  type Bwd (Wishbone dom mode addressWidth dat) = Signal dom (WishboneS2M dat)


instance Backpressure (Wishbone dom mode addressWidth dat) where
  boolsToBwd _ = fromList_lazy . Prelude.map (\b -> wishboneS2M { acknowledge = b })


instance (KnownNat (BitSize dat), KnownDomain dom) => Simulate (Wishbone dom mode addressWidth dat) where
  type SimulateFwdType (Wishbone dom mode addressWidth dat) = [WishboneM2S addressWidth (BitSize dat `DivRU` 8) dat]

  type SimulateBwdType (Wishbone dom mode addressWidth dat) = [WishboneS2M dat]

  type SimulateChannels (Wishbone dom mode addressWidth dat) = 1

  simToSigFwd Proxy = fromList_lazy
  simToSigBwd Proxy = fromList_lazy
  sigToSimFwd Proxy = sample_lazy
  sigToSimBwd Proxy = sample_lazy

  stallC config (head -> (stallAck, stalls)) = Circuit $
      uncurry (go stallAcks stalls (resetCycles config))
    where
      stallAcks
        | stallAck == StallCycle = [minBound..maxBound] \\ [StallCycle]
        | otherwise = [stallAck]

      toStallReply
        :: WishboneM2S addressWidth (BitSize dat `DivRU` 8) dat
        -> WishboneS2M dat
        -> StallAck
        -> WishboneS2M dat
      toStallReply WishboneM2S{..} s2m ack
        | strobe && busCycle = s2m
        | otherwise = case ack of
            StallWithNack      -> s2m { acknowledge = False }
            StallWithAck       -> s2m { acknowledge = True }
            StallWithErrorX    -> errorX "No defined ack"
            StallTransparently -> s2m
            StallCycle         -> s2m { acknowledge = False } -- shouldn't happen

      go
        :: [StallAck]
        -> [Int]
        -> Int
        -> Signal dom (WishboneM2S addressWidth (BitSize dat `DivRU` 8) dat)
        -- ^ input from master
        -> Signal dom (WishboneS2M dat)
        -- ^ reply from slave
        -> ( Signal dom (WishboneS2M dat)
           , Signal dom (WishboneM2S addressWidth (BitSize dat `DivRU` 8) dat))
      -- "refill" stall acks and continue
      go [] ss rs fwd bwd = go stallAcks ss rs fwd bwd

      -- perform resets
      go (_:sas) _ resetN (f :- fwd) ~(b :- bwd) | resetN > 0 =
        B.bimap (b :-) (f :-) (go sas stalls (resetN - 1) fwd bwd)

      go (sa:sas) [] _ (f :- fwd) ~(b :- bwd) =
        B.bimap (toStallReply f b sa :-) (f :-) (go sas [] 0 fwd bwd)

      go (sa:sas) ss _ (f :- fwd) ~(b :- bwd)
        | not (busCycle f && strobe f)
        =
          -- Left hand side does not send data, simply replicate that behavior. Right
          -- hand side might send an arbitrary acknowledgement, so we simply pass it
          -- through.

          -- `f` does not signal/send any data, so it's an "empty" reply.
          B.bimap (toStallReply f b sa :-) (f :-) (go sas ss 0 fwd bwd)

      go (_sa:sas) (s:ss) _ (f0 :- fwd) ~(b0 :- bwd) =
        let
          -- Stall a,0s long as s > 0. If s ~ 0, we wait for the RHS to acknowledge
          -- the data. As long as RHS does not acknowledge the data, we keep sending
          -- the same data.
          (f1, b1, s1) = case compare 0 s of
            LT -> (f0 { strobe = False, busCycle = False }, b0 { acknowledge = False }, pred s:ss)    -- s > 0
            EQ -> (f0, b0, if acknowledge b0 then ss else s:ss) -- s ~ 0
            GT -> error ("Unexpected negative stall: " <> show s) -- s < 0
        in
          B.bimap (b1 :-) (f1 :-) (go sas s1 0 fwd bwd)


data WishboneState
  = Quiet
  | InCycleNoStrobe
  | WaitForSlave
  -- TODO add a SlaveHolding state? Spec seems unclear

data WishboneError
  = MoreThanOneTerminationSignalAsserted
  | TerminationSignalOutsideOfCycle


nextStateStandard
  :: WishboneState
  -> WishboneM2S addressWidth (BitSize a `DivRU` 8) a
  -> WishboneS2M a
  -> Either WishboneError WishboneState
nextStateStandard _ m2s s2m
  | length (filter ($ s2m) [acknowledge, err, retry]) > 1           = Left MoreThanOneTerminationSignalAsserted
  | not (busCycle m2s) && (acknowledge s2m || err s2m || retry s2m) = Left TerminationSignalOutsideOfCycle
nextStateStandard Quiet m2s _
  | busCycle m2s && not (strobe m2s)        = Right InCycleNoStrobe
  | busCycle m2s && strobe m2s              = Right WaitForSlave
  | otherwise                               = Right Quiet
nextStateStandard InCycleNoStrobe m2s _
  | not (busCycle m2s)         = Right Quiet
  | busCycle m2s && strobe m2s = Right WaitForSlave
  | otherwise                  = Right InCycleNoStrobe
nextStateStandard WaitForSlave m2s s2m
  | busCycle m2s && not (strobe m2s)        = Right InCycleNoStrobe
  | not (busCycle m2s)                      = Right Quiet
  | acknowledge s2m || err s2m || retry s2m = Right Quiet
  | otherwise                               = Right WaitForSlave


-- | Validate the input/output streams of a standard wishbone interface
--
-- Returns 'Right ()' when the interactions comply with the spec.
-- Returns 'Left (cycle, err)' when a spec violation was found.
validateStandard :: [WishboneM2S addressWidth (BitSize a `DivRU` 8) a] -> [WishboneS2M a] -> Either (Int, WishboneError) ()
validateStandard m2s s2m = go 0 (Prelude.zip m2s s2m) Quiet
  where
    go _ [] _                 = Right ()
    go n ((fwd, bwd):rest) st = case nextStateStandard st fwd bwd of
      Left err  -> Left (n, err)
      Right st' -> go (n + 1) rest st'

wishboneM2S :: WishboneM2S addressWidth selWidth dat
wishboneM2S
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

wishboneS2M :: WishboneS2M dat
wishboneS2M
  = WishboneS2M
  { readData = Clash.Prelude.undefined
  , acknowledge = False
  , err = False
  , retry = False
  , stall = False
  }

-- | Wishbone to Df source
--
-- * Writing to the given address, pushes an item onto the fifo
-- * Reading always returns zero
-- * Writing to other addresses are acknowledged, but ignored
-- * Asserts stall when the FIFO is full
wishboneSource ::
  (1 + n) ~ depth =>
  HiddenClockResetEnable dom =>
  KnownNat addressWidth =>
  KnownNat depth =>
  KnownNat (BitSize dat) =>
  NFDataX dat =>
  -- | Address to respond to
  BitVector addressWidth ->
  -- | Depth of the FIFO
  SNat depth ->
  -- |
  (Signal dom (WishboneM2S addressWidth selWidth dat), Signal dom Ack) ->
  -- |
  (Signal dom (WishboneS2M dat), Signal dom (Data dat))
wishboneSource respondAddress fifoDepth = unbundle . mealy machineAsFunction s0 . addResetInp . bundle where

  addResetInp inp = hideReset (\(Reset r) -> bundle (r, inp))

  machineAsFunction s i = (s',o) where (o,s') = runState (fullStateMachine i) s

  s0 = (NoData, E.replicate fifoDepth NoData)

  fullStateMachine (True, _) = pure (wishboneS2M, NoData) -- reset is on, don't output anything or change anything
  fullStateMachine (False, (m2s, (Ack ack))) = do
    leftOtp <- leftStateMachine m2s
    rightOtp <- gets fst
    when ack $ modify rightAck
    pure (leftOtp, rightOtp)

  leftStateMachine m2s
    | strobe m2s && addr m2s == respondAddress && writeEnable m2s = pushInput (writeData m2s)
    | strobe m2s = pure (wishboneS2M { acknowledge = True })
    | otherwise = pure wishboneS2M

  pushInput inpData = do
    (currOtp, buf) <- get
    case (currOtp, E.last buf) of
      (NoData, _) -> put (Data inpData, buf)             >> pure (wishboneS2M { acknowledge = True })
      (_, NoData) -> put (currOtp, Data inpData +>> buf) >> pure (wishboneS2M { acknowledge = True })
      _ -> pure (wishboneS2M { stall = True })

  rightAck (_, buf) = (E.head buf, buf <<+ NoData)

-- | Wishbone to Df sink
--
-- * Reading from the given address, pops an item from the fifo
-- * Writes are acknowledged, but ignored
-- * Reads from any other address are acknowledged, but the fifo element is not popped.
-- * Asserts stall when the FIFO is empty
wishboneSink ::
  (1 + n) ~ depth =>
  HiddenClockResetEnable dom =>
  KnownNat addressWidth =>
  KnownNat depth =>
  KnownNat (BitSize dat) =>
  NFDataX dat =>
  -- | Address to respond to
  BitVector addressWidth ->
  -- | Depth of the FIFO
  SNat depth ->
  -- |
  (Signal dom (Data dat), Signal dom (WishboneM2S addressWidth selWidth dat)) ->
  -- |
  (Signal dom Ack, Signal dom (WishboneS2M dat))
wishboneSink respondAddress fifoDepth = unbundle . mealy machineAsFunction s0 . addResetInp . bundle where

  addResetInp inp = hideReset (\(Reset r) -> bundle (r, inp))

  machineAsFunction s i = (s',o) where (o,s') = runState (fullStateMachine i) s

  s0 = (Nothing, E.replicate fifoDepth NoData)

  fullStateMachine (True, _) = pure (Ack False, wishboneS2M) -- reset is on, don't output anything or change anything
  fullStateMachine (False, (inpData, m2s)) = do
    leftOtp <- leftStateMachine inpData
    rightStateMachine m2s
    rightOtp <- gets (fromMaybe wishboneS2M . fst)
    pure (leftOtp, rightOtp)

  leftStateMachine NoData = pure (Ack False)
  leftStateMachine inpData@(Data _) = do
    buf <- gets snd
    case (E.last buf) of
      NoData -> modify (second $ const $ inpData +>> buf) >> pure (Ack True)
      _ -> pure (Ack False)

  rightStateMachine m2s
    | not (strobe m2s && addr m2s == respondAddress && not (writeEnable m2s)) = modify $ first $ const Nothing
    | otherwise = do
        (otp, buf) <- get
        case (otp, E.head buf) of
          (Nothing, Data toSend) -> put (Just $ wishboneS2M { acknowledge = True, readData = toSend }, buf <<+ NoData)
          (Just otp', Data toSend) -> if not (stall otp') then pure () else put (Just $ wishboneS2M { acknowledge = True, readData = toSend }, buf <<+ NoData)
          (Nothing, NoData) -> put (Just $ wishboneS2M { stall = True }, buf)
          _ -> pure ()
