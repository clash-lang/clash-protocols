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
import           Prelude                    hiding (head, not, (&&), reverse)
import           Protocols.Hedgehog         (ExpectOptions (..), Test (..))
import           Protocols.Internal
import           Protocols.Df               (Df, Data(..))

-- hedgehog
import qualified Hedgehog                   as H
import qualified Hedgehog.Internal.Property as H
import qualified Hedgehog.Internal.Show     as H
import           Text.Show.Pretty           (ppShow)

import qualified Clash.Explicit.Prelude     as E
import           Control.Monad.State        (get, gets, modify, put, runState)
import           Control.Monad              (when)
import           Data.Either                (fromRight)
import           Data.Maybe                 (isNothing, fromJust)

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
-- * Reading from statusAddress returns how much free space is left in the buffer
-- * Reading/writing to other addresses are acknowledged, but ignored
-- * Asserts err and stall when the fifo is full
wishboneSource ::
  (1 + n) ~ depth =>
  HiddenClockResetEnable dom =>
  KnownNat addressWidth =>
  KnownNat depth =>
  KnownNat (BitSize dat) =>
  NFDataX dat =>
  -- | Address to respond to
  BitVector addressWidth ->
  -- | Optional address for asking how much space is left in the buffer
  Maybe (BitVector addressWidth) ->
  -- | Depth of the FIFO
  SNat depth ->
  -- |
  Circuit (Wishbone dom mode addressWidth (Either (Index (depth+1)) dat)) (Df dom dat)
wishboneSource respondAddress statusAddress fifoDepth = Circuit (hideReset circuitFunction) where

  -- implemented using a fixed-size array
  --   write location and read location are both stored
  --   to write, write to current location and move one to the right
  --   to read, read from current location and move one to the right
  --   loop around from the end to the beginning if necessary

  circuitFunction reset (inpL, inpR) = (otpL, otpR) where
    brRead = readNew (blockRam (E.replicate fifoDepth $ errorX "wishboneSource: undefined initial fifo buffer value")) brReadAddr brWrite
    (brReadAddr, brWrite, otpL, otpR) = unbundle $ mealy machineAsFunction s0 $ bundle (brRead, unsafeToHighPolarity reset, inpL, inpR)

  machineAsFunction s i = (s',o) where (o,s') = runState (fullStateMachine i) s

  -- initial state
  -- (left status output, right data output, amount of space left, next place to read from, next place to write to)
  s0 = let numFree = maxBound
           nextRW = numFree * 0
           -- extremely janky;
           --   forces nextRead and nextWrite to have the same type (that is, Index (depth+1)) as numFree
           --   (since ghc wouldn't be able to tell their type otherwise)
           -- nextRW is initialized to 0, although it could take on any value and the buffer would still work
       in
           (Nothing, NoData, numFree, nextRW, nextRW)

  fullStateMachine (_, True, _, _) = pure (0, Nothing, wishboneS2M, NoData) -- reset is on, don't output anything or change anything
  fullStateMachine (brRead, False, m2s, (Ack ack)) = do
    (brReadAddr, rightOtp) <- rightStateMachine brRead
    (brWrite, leftOtp) <- leftStateMachine m2s
    when ack $ modify removeDataOtp
    pure (brReadAddr, brWrite, leftOtp, rightOtp)

  -- given wishbone input, decide wishbone output
  leftStateMachine m2s
    | strobe m2s && addr m2s == respondAddress && writeEnable m2s = modify removeFifoStatusInfo >> pushInput (fromRight Clash.Prelude.undefined $ writeData m2s)
    | strobe m2s && Just (addr m2s) == statusAddress && not (writeEnable m2s) = do
        (otpL, otpR, numFree, nextRead, nextWrite) <- get
        when (isNothing otpL) $ put (Just numFree, otpR, numFree, nextRead, nextWrite)
        (otpL', _, _, _, _) <- get
        pure (Nothing, wishboneS2M { acknowledge = True, readData = Left (fromJust otpL') })
    | otherwise = modify removeFifoStatusInfo >> pure (Nothing, wishboneS2M { acknowledge = strobe m2s })

  pushInput inpData = do
    (currOtpL, currOtpR, numFree, nextRead, nextWrite) <- get
    if numFree == 0 then pure (Nothing, wishboneS2M { err = True, stall = True }) else do
      put (currOtpL, currOtpR, numFree-1, nextRead, incIdxLooping nextWrite)
      pure (Just (nextWrite, inpData), wishboneS2M { acknowledge = True })

  -- given df input, decide df output
  rightStateMachine brRead = do
    (_,rightOtp,_,_,_) <- get
    (currOtpL, currOtpR, numFree, nextRead, nextWrite) <- get
    case (currOtpR, numFree == maxBound) of
      (NoData, False) -> put (currOtpL, Data brRead, numFree+1, incIdxLooping nextRead, nextWrite)
      _ -> pure ()
    (_,_,_,brReadAddr,_) <- get
    pure (brReadAddr, rightOtp)

  removeFifoStatusInfo (_,b,c,d,e) = (Nothing,b,c,d,e)
  removeDataOtp (a,_,c,d,e) = (a,NoData,c,d,e)

  -- we have Index (depth+1) but we only want to access blockram up to depth-1
  incIdxLooping idx = if idx >= (maxBound-1) then 0 else idx+1

-- | Wishbone to Df sink
--
-- * Reading from the given address, pops an item from the fifo
-- * Writes are acknowledged, but ignored
-- * Reading from statusAddress returns how much free space is left in the buffer
-- * Reading/writing to any other address is acknowledged, but the fifo element is not popped.
-- * Asserts err when the FIFO is empty
wishboneSink ::
  (1 + n) ~ depth =>
  HiddenClockResetEnable dom =>
  KnownNat addressWidth =>
  KnownNat depth =>
  KnownNat (BitSize dat) =>
  NFDataX dat =>
  -- | Address to respond to
  BitVector addressWidth ->
  -- | Optional address for asking how much space is left in the buffer
  Maybe (BitVector addressWidth) ->
  -- | Depth of the FIFO
  SNat depth ->
  -- |
  Circuit (Df dom dat) (Reverse (Wishbone dom mode addressWidth (Either (Index (depth+1)) dat)))
wishboneSink respondAddress statusAddress fifoDepth = Circuit (hideReset circuitFunction) where

  -- implemented using a fixed-size array
  --   write location and read location are both stored
  --   to write, write to current location and move one to the right
  --   to read, read from current location and move one to the right
  --   loop around from the end to the beginning if necessary

  circuitFunction reset (inpL, inpR) = (otpL, otpR) where
    brRead = readNew (blockRam (E.replicate fifoDepth $ errorX "wishboneSink: undefined initial fifo buffer value")) brReadAddr brWrite
    (brReadAddr, brWrite, otpL, otpR) = unbundle $ mealy machineAsFunction s0 $ bundle (brRead, unsafeToHighPolarity reset, inpL, inpR)

  machineAsFunction s i = (s',o) where (o,s') = runState (fullStateMachine i) s

  -- initial state
  -- (right output (amt left or data), amount of space left, next place to read from, next place to write to)
  s0 = let numFree = maxBound
           nextRW = numFree * 0
           -- extremely janky;
           --   forces nextRead and nextWrite to have the same type (that is, Index (depth+1)) as numFree
           --   (since ghc wouldn't be able to tell their type otherwise)
           -- nextRW is initialized to 0, although it could take on any value and the buffer would still work
       in
           (Nothing, numFree, nextRW, nextRW)

  -- when reset is on, output blank/default and don't change any state
  fullStateMachine (_, True, _, _) = pure (0, Nothing, Ack False, wishboneS2M) -- reset is on, don't output anything or change anything
  fullStateMachine (brRead, False, inpData, m2s) = do
    rightStateMachine m2s brRead
    -- fix some outputs before they get changed for next time later on
    (rightOtp, _, brReadAddr, _) <- get
    (brWrite, leftOtp) <- leftStateMachine inpData
    pure (brReadAddr, brWrite, leftOtp, fromMaybe wishboneS2M rightOtp)

  -- given df input, decide df output
  leftStateMachine NoData = pure (Nothing, Ack False)
  leftStateMachine (Data inpData) = do
    (otp, numFree, nextRead, nextWrite) <- get
    if numFree == 0 then pure (Nothing, Ack False) else do
      put (otp, numFree-1, nextRead, incIdxLooping nextWrite)
      pure (Just (nextWrite, inpData), Ack True)

  -- given wishbone input, decide wishbone output (store in state)
  rightStateMachine m2s brRead
    | strobe m2s && addr m2s == respondAddress && not (writeEnable m2s) = do
        (otp, numFree, nextRead, nextWrite) <- get
        case (otp, numFree == maxBound) of
          (Nothing, False) ->
            put (Just $ wishboneS2M { acknowledge = True, readData = Right brRead }, numFree+1, incIdxLooping nextRead, nextWrite)
          (Just otp', False) ->
            if not (err otp') then pure () else put (Just $ wishboneS2M { acknowledge = True, readData = Right brRead }, numFree+1, incIdxLooping nextRead, nextWrite)
          (Nothing, True) ->
            put (Just $ wishboneS2M { err = True }, numFree, nextRead, nextWrite)
          _ -> pure ()
    | strobe m2s && Just (addr m2s) == statusAddress && not (writeEnable m2s) = do
        (otp, numFree, nextRead, nextWrite) <- get
        when (isNothing otp) $ put (Just $ wishboneS2M { acknowledge = True, readData = Left numFree }, numFree, nextRead, nextWrite)
    | otherwise = do
        (_, x, y, z) <- get
        put (Nothing, x, y, z)

  -- we have Index (depth+1) but we only want to access blockram up to depth-1
  incIdxLooping idx = if idx == (maxBound-1) then 0 else idx+1
