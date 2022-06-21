{-|
Defines a mix-and-match interface for creating fifo buffers.
Buffers can be made from one protocol to another,
and are parametrized on the amount of items in the buffer.
Blockram is used to store fifo buffer items.
-}

{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, NamedFieldPuns, UndecidableInstances #-}

module Protocols.Fifo where

import           Prelude hiding (replicate)
import           Control.Monad (when)
import           Control.Monad.State (StateT(..), State, runState, get, put, gets, modify)
import           Clash.Prelude hiding ((&&), (||), not)
import           Data.Functor.Identity (Identity(..))
import           Data.Maybe (isJust, fromJust, isNothing)
import           Data.Proxy (Proxy(..))

-- me
import           Protocols.Axi4.Common (KeepBurst(..), KeepSize(..), KeepBurstLength(..), KeepResponse(..), KeepStrobe(..), Width, BurstMode(..), Resp(..))
import           Protocols.Axi4.ReadAddress (M2S_ReadAddress(..), S2M_ReadAddress(..))
import           Protocols.Axi4.ReadData (M2S_ReadData(..), S2M_ReadData(..))
import           Protocols.Axi4.WriteAddress (M2S_WriteAddress(..), S2M_WriteAddress(..))
import           Protocols.Axi4.WriteData (M2S_WriteData(..), S2M_WriteData(..))
import           Protocols.Axi4.WriteResponse (M2S_WriteResponse(..), S2M_WriteResponse(..))
import           Protocols.Df (Data(..))
import           Protocols.Internal


-- | Protocols that can be used on the input side of a fifo buffer with a given datatype and depth.
-- Parameters: forward and backward signals carried by protocols; data carried; and fifo depth
class (NFDataX (FifoInpState fwd bwd dat depth), NFDataX dat, KnownNat depth) => FifoInput fwd bwd dat (depth :: Nat) where
  -- | State carried between clock cycles
  type FifoInpState fwd bwd dat depth
  -- | User-provided parameters for the fifo input
  type FifoInpParam fwd bwd dat depth
  -- | Initial state, given depth and user params
  fifoInpS0 :: Proxy (fwd,bwd,dat) -> SNat depth -> FifoInpParam fwd bwd dat depth -> FifoInpState fwd bwd dat depth
  -- | Blank input, used when reset is on.
  -- Doesn't look at current state, but can look at depth and user params.
  -- Should not acknowledge any incoming data
  fifoInpBlank :: Proxy (fwd,bwd,dat) -> SNat depth -> FifoInpParam fwd bwd dat depth -> bwd
  -- | State machine run every clock cycle at the fifo input port.
  -- Given user-provided params; data at the input port; and current amount of space left in the buffer.
  -- Can update state using State monad.
  -- Returns data to output back to the port (usually an acknowledge signal), and Maybe an item to put into the fifo buffer.
  -- Do not push any data to the buffer if space left == 0;
  --   doing so will cause potential data loss and integer overflow
  fifoInpFn :: Proxy (fwd,bwd,dat) -> SNat depth -> FifoInpParam fwd bwd dat depth -> fwd -> Index (depth+1) -> State (FifoInpState fwd bwd dat depth) (bwd, Maybe dat)

-- | Protocols that can be used on the input side of a fifo buffer with a given datatype and depth.
-- Parameters: forward and backward signals carried by protocols; data carried; and fifo depth
class (NFDataX (FifoOtpState fwd bwd dat depth), NFDataX dat, KnownNat depth) => FifoOutput fwd bwd dat (depth :: Nat) where
  -- | State carried between clock cycles
  type FifoOtpState fwd bwd dat depth
  -- | User-provided parameters for the fifo output
  type FifoOtpParam fwd bwd dat depth
  -- | Initial state, given depth and user params
  fifoOtpS0 :: Proxy (fwd,bwd,dat) -> SNat depth -> FifoOtpParam fwd bwd dat depth -> FifoOtpState fwd bwd dat depth
  -- | Blank input, used when reset is on.
  -- Doesn't look at current state, but can look at depth and user params.
  -- Should not acknowledge any incoming data
  fifoOtpBlank :: Proxy (fwd,bwd,dat) -> SNat depth -> FifoOtpParam fwd bwd dat depth -> fwd
  -- | State machine run every clock cycle at the fifo output port.
  -- Given user-provided params; data at the output port (usually an acknowledge signal); current amount of space left in the buffer; and the next data item on the buffer.
  -- Can update state using State monad.
  -- Returns data to output back to the port (usually data taken from the buffer), and whether a data item was taken from the buffer.
  -- Do not take any data from the buffer (or even read the top buffer value) if space left == maxBound;
  --   doing so will cause potential data loss, integer overflow, and reading an undefined value
  fifoOtpFn :: Proxy (fwd,bwd,dat) -> SNat depth -> FifoOtpParam fwd bwd dat depth -> bwd -> Index (depth+1) -> dat -> State (FifoOtpState fwd bwd dat depth) (fwd, Bool)


-- | Generalized fifo (see classes above).
-- Uses blockram to store data
fifo ::
  HiddenClockResetEnable dom =>
  KnownNat depth =>
  NFDataX dat =>
  FifoInput fwdA bwdA dat depth =>
  FifoOutput fwdB bwdB dat depth =>
  FifoInpState fwdA bwdA dat depth ~ sA =>
  FifoOtpState fwdB bwdB dat depth ~ sB =>
  Proxy (fwdA,bwdA,dat) ->
  Proxy (fwdB,bwdB,dat) ->
  SNat depth ->
  FifoInpParam fwdA bwdA dat depth ->
  FifoOtpParam fwdB bwdB dat depth ->
  (Signal dom fwdA, Signal dom bwdB) ->
  (Signal dom bwdA, Signal dom fwdB)
fifo pxyA pxyB fifoDepth paramA paramB = hideReset circuitFunction where

  -- implemented using a fixed-size array
  --   write location and read location are both stored
  --   to write, write to current location and move one to the right
  --   to read, read from current location and move one to the right
  --   loop around from the end to the beginning if necessary

  circuitFunction reset (inpA, inpB) = (otpA, otpB) where
    -- initialize bram
    brRead = readNew (blockRam (replicate fifoDepth $ errorX "fifo: undefined initial fifo buffer value")) brReadAddr brWrite
    -- run the state machine (a mealy machine)
    (brReadAddr, brWrite, otpA, otpB) = unbundle $ mealy machineAsFunction s0 $ bundle (brRead, unsafeToHighPolarity reset, inpA, inpB)

  -- when reset is on, set state to initial state and output blank outputs
  machineAsFunction _ (_, True, _, _) = (s0, (0, Nothing, fifoInpBlank pxyA fifoDepth paramA, fifoOtpBlank pxyB fifoDepth paramB))
  machineAsFunction (sA,sB,rAddr,wAddr,amtLeft) (brRead, False, iA, iB) =
    let -- run the input port state machine
        ((oA, maybePush), sA') = runState (fifoInpFn pxyA fifoDepth paramA iA amtLeft) sA
        -- potentially push an item onto blockram
        brWrite = (wAddr,) <$> maybePush
        -- adjust write address and amount left (output state machine doesn't see amountLeft')
        (wAddr', amtLeft') = if (isJust maybePush) then (incIdxLooping wAddr, amtLeft-1) else (wAddr, amtLeft)
        -- if we're about to push onto an empty queue, we can pop immediately instead
        (brRead_, amtLeft_) = if (amtLeft == maxBound && isJust maybePush) then (fromJust maybePush, amtLeft') else (brRead, amtLeft)
        -- run the output port state machine
        ((oB, popped), sB') = runState (fifoOtpFn pxyB fifoDepth paramB iB amtLeft_ brRead_) sB
        -- adjust blockram read address and amount left
        (rAddr', amtLeft'') = if popped then (incIdxLooping rAddr, amtLeft'+1) else (rAddr, amtLeft')
        brReadAddr = rAddr'
        -- return our new state and outputs
    in  ((sA', sB', rAddr', wAddr', amtLeft''), (brReadAddr, brWrite, oA, oB))

  -- initial state
  -- (s0 for input port (taken from class), s0 for output port (taken from class), next read address, next write address, space left in bram)
  s0 = (fifoInpS0 pxyA fifoDepth paramA, fifoOtpS0 pxyB fifoDepth paramB, _0 fifoDepth, _0 fifoDepth, _maxBound fifoDepth)

  -- type level hack
  -- make sure we have the right Index number
  _0 :: (KnownNat n) => SNat n -> Index n
  _0 = const 0

  -- type level hack
  -- make sure we have the right Index number
  _maxBound :: (KnownNat n) => SNat n -> Index (n+1)
  _maxBound = const maxBound

  -- loop around to 0 if we're about to overflow, otherwise increment
  incIdxLooping idx = if idx == maxBound then 0 else idx+1


-- Fifo classes for Df

instance (NFDataX dat, KnownNat depth) => FifoInput (Data dat) Ack dat depth where
  type FifoInpState (Data dat) Ack dat depth = ()
  type FifoInpParam (Data dat) Ack dat depth = ()
  fifoInpS0 _ _ _ = ()
  fifoInpBlank _ _ _ = Ack False
  fifoInpFn _ _ _ (Data inp) n | n > 0 = pure (Ack True, Just inp)
  fifoInpFn _ _ _ _ _ = pure (Ack False, Nothing)

instance (NFDataX dat, KnownNat depth) => FifoOutput (Data dat) Ack dat depth where
  type FifoOtpState (Data dat) Ack dat depth = Maybe dat
  type FifoOtpParam (Data dat) Ack dat depth = ()
  fifoOtpS0 _ _ _ = Nothing
  fifoOtpBlank _ _ _ = NoData
  fifoOtpFn _ _ _ (Ack ack) amtLeft queueItem = do
    sending <- get
    retVal <- case (sending, amtLeft == maxBound) of
      (Just toSend, _) -> pure (Data toSend, False)
      (Nothing, False) -> put (Just queueItem) >> pure (Data queueItem, True)
      (Nothing, True) -> pure (NoData, False)
    shouldReadAck <- gets isJust -- ack might be undefined, so we shouldn't look at it unless we have to
    when (shouldReadAck && ack) $ put Nothing
    pure retVal


-- Fifo classes for Axi4

-- Fifo input for Axi4
-- Write channels are used for receiving data
-- Read channels are used to send status info (how much space is left in the buffer)
-- The user can provide an address for sending data, an address for reading status, and values to be sent over the user channels
instance (NFDataX wrUser, NFDataX rdUser, KnownNat wdBytes, KnownNat dp1, dp1 ~ (depth + 1), KnownNat (Width aw), KnownNat (Width w_iw), KnownNat (Width r_iw)) =>
  FifoInput
    (M2S_WriteAddress 'KeepBurst waKeepSize w_lw w_iw aw waKeepRegion waKeepBurstLength waKeepLock waKeepCache waKeepPermissions waKeepQos waUser,
     M2S_WriteData 'KeepStrobe wdBytes wdUser,
     M2S_WriteResponse,
     M2S_ReadAddress 'KeepBurst 'NoSize r_lw r_iw aw raKeepRegion 'KeepBurstLength raKeepLock raKeepCache raKeepPermissions raKeepQos raUser,
     M2S_ReadData)
    (S2M_WriteAddress,
     S2M_WriteData,
     S2M_WriteResponse 'KeepResponse w_iw wrUser,
     S2M_ReadAddress,
     S2M_ReadData 'KeepResponse r_iw rdUser (Index dp1))
    (Vec wdBytes (Maybe (BitVector 8)))
    depth
    where

  type FifoInpState
    (M2S_WriteAddress 'KeepBurst waKeepSize w_lw w_iw aw waKeepRegion waKeepBurstLength waKeepLock waKeepCache waKeepPermissions waKeepQos waUser,
     M2S_WriteData 'KeepStrobe wdBytes wdUser,
     M2S_WriteResponse,
     M2S_ReadAddress 'KeepBurst 'NoSize r_lw r_iw aw raKeepRegion 'KeepBurstLength raKeepLock raKeepCache raKeepPermissions raKeepQos raUser,
     M2S_ReadData)
    (S2M_WriteAddress,
     S2M_WriteData,
     S2M_WriteResponse 'KeepResponse w_iw wrUser,
     S2M_ReadAddress,
     S2M_ReadData 'KeepResponse r_iw rdUser (Index dp1))
    (Vec wdBytes (Maybe (BitVector 8)))
    depth
    = (Maybe (BitVector (Width w_iw)), S2M_WriteResponse 'KeepResponse w_iw wrUser, Index (2^8), BitVector (Width r_iw), S2M_ReadData 'KeepResponse r_iw rdUser (Index dp1))
  -- (Just write id if we're being written to (otherwise Nothing), write response, burst length left for send status output, status output read id, status output)

  type FifoInpParam
    (M2S_WriteAddress 'KeepBurst waKeepSize w_lw w_iw aw waKeepRegion waKeepBurstLength waKeepLock waKeepCache waKeepPermissions waKeepQos waUser,
     M2S_WriteData 'KeepStrobe wdBytes wdUser,
     M2S_WriteResponse,
     M2S_ReadAddress 'KeepBurst 'NoSize r_lw r_iw aw raKeepRegion 'KeepBurstLength raKeepLock raKeepCache raKeepPermissions raKeepQos raUser,
     M2S_ReadData)
    (S2M_WriteAddress,
     S2M_WriteData,
     S2M_WriteResponse 'KeepResponse w_iw wrUser,
     S2M_ReadAddress,
     S2M_ReadData 'KeepResponse r_iw rdUser (Index dp1))
    (Vec wdBytes (Maybe (BitVector 8)))
    depth
    = (BitVector (Width aw), Maybe (BitVector (Width aw)), wrUser, rdUser)
    -- write data address, read status address, user response for write, user response for read

  fifoInpS0 _ _ _ =
    (Nothing,
     S2M_NoWriteResponse,
     0,
     errorX "FifoInput for Axi4: No initial value for read id",
     S2M_NoReadData)

  fifoInpBlank _ _ _ = (S2M_WriteAddress{_awready = False}, S2M_WriteData{_wready = False}, S2M_NoWriteResponse, S2M_ReadAddress{_arready = False}, S2M_NoReadData)

  fifoInpFn _ _ (dataAddr,statusAddr,wrUser,rdUser) (wAddrVal, wDataVal, wRespAck, rAddrVal, rDataAck) amtLeft = makeState stateFn where

    makeState :: (s -> (a,s)) -> State s a
    makeState f = StateT (Identity . f)

    stateFn (writeId, writeResp, readBurstLenLeft, readId, readData)
      = let (((wAddrAck, wDataAck, wRespVal), writeVal), (writeId', writeResp')) = runState writeStateMachine (writeId, writeResp)
            ((rAddrAck, rDataVal), (readBurstLenLeft', readId', readData')) = runState readStateMachine (readBurstLenLeft, readId, readData)
        in  (((wAddrAck, wDataAck, wRespVal, rAddrAck, rDataVal), writeVal), (writeId', writeResp', readBurstLenLeft', readId', readData'))

    writeStateMachine = do
      wAddrAck <- processWAddr wAddrVal
      (wDataAck, toPop) <- processWData wDataVal
      wRespVal <- gets snd
      processWRespAck
      pure ((wAddrAck, wDataAck, wRespVal), toPop)

    processWAddr M2S_NoWriteAddress = pure (S2M_WriteAddress{_awready = False})
    processWAddr M2S_WriteAddress{ _awburst } | _awburst /= BmFixed = pure (S2M_WriteAddress{_awready = True})
    processWAddr M2S_WriteAddress{_awaddr, _awid} = do
      (_,b) <- get
      put (if _awaddr == dataAddr then Just _awid else Nothing, b)
      pure (S2M_WriteAddress{_awready = True})

    processWData M2S_NoWriteData = pure (S2M_WriteData{_wready = False}, Nothing)
    processWData M2S_WriteData{_wlast, _wdata} = do
      (shouldRead,respS2M) <- get
      -- we only want to output _wready = false if we're the recpient of the writes AND our buffer is full
      -- we only want to push if we're the recpient of the writes AND our buffer has space available
      if (isNothing shouldRead || amtLeft == 0) then pure (S2M_WriteData{_wready = isNothing shouldRead}, Nothing) else do
        put (Nothing,
             if _wlast then S2M_WriteResponse {_bid = fromJust shouldRead, _bresp = ROkay, _buser = wrUser } else respS2M)
        pure (S2M_WriteData{_wready = True}, Just _wdata)

    processWRespAck = when (_bready wRespAck) $ modify (\(a,_) -> (a,S2M_NoWriteResponse))

    readStateMachine = do
      rAddrAck <- processRAddr rAddrVal
      sendRData
      (_,_,rDataVal) <- get
      clearRData rDataAck
      pure (rAddrAck, rDataVal)

    -- if state is being asked for, log the burst length requested
    processRAddr M2S_NoReadAddress = pure (S2M_ReadAddress { _arready = False })
    processRAddr M2S_ReadAddress{ _arburst } | _arburst /= BmFixed = pure (S2M_ReadAddress{ _arready = True })
    processRAddr M2S_ReadAddress{ _araddr, _arlen, _arid } = do
      (burstLenLeft,_,c) <- get
      when (Just _araddr == statusAddr && burstLenLeft == 0) $ put (_arlen,_arid,c)
      pure (S2M_ReadAddress { _arready = burstLenLeft /= 0 })

    -- write down what status message we're sending (so it doesn't change between clock cycles)
    sendRData = do
      (burstLenLeft,rid,statusOut) <- get
      -- case (our status is requested, we aren't already giving it) of
      case (burstLenLeft == 0, statusOut) of
        (False, S2M_NoReadData) -> put (burstLenLeft-1,rid,S2M_ReadData { _rdata = amtLeft, _rid = rid, _rresp = ROkay, _rlast = burstLenLeft == 1, _ruser = rdUser })
        _ -> pure ()

    clearRData M2S_ReadData{ _rready } = when _rready $ do
      (a,b,_) <- get
      put (a, b, S2M_NoReadData)


-- Fifo output for Axi4
-- The user can provide an address for sending data, an address for reading status (optional), and values to be sent over the user channels
instance (NFDataX dat, NFDataX rdUser, KnownNat dp1, dp1 ~ (depth + 1), KnownNat (Width aw), KnownNat (Width iw)) =>
  FifoOutput
    (S2M_ReadAddress,
     S2M_ReadData 'KeepResponse iw rdUser (Either (Index dp1) dat))
    (M2S_ReadAddress 'KeepBurst 'NoSize lw iw aw keepRegion 'KeepBurstLength keepLock keepCache keepPermissions keepQos raData,
     M2S_ReadData)
    dat
    depth
    where

  type FifoOtpState
    (S2M_ReadAddress,
     S2M_ReadData 'KeepResponse iw rdUser (Either (Index dp1) dat))
    (M2S_ReadAddress 'KeepBurst 'NoSize lw iw aw keepRegion 'KeepBurstLength keepLock keepCache keepPermissions keepQos raData,
     M2S_ReadData)
    dat
    depth
    = (Bool, Index (2^8), BitVector (Width iw), S2M_ReadData 'KeepResponse iw rdUser (Either (Index dp1) dat))
    -- (what we're sending: status (false) or data (true), burst length left, read id, read data currently sending)

  type FifoOtpParam
    (S2M_ReadAddress,
     S2M_ReadData 'KeepResponse iw rdUser (Either (Index dp1) dat))
    (M2S_ReadAddress 'KeepBurst 'NoSize lw iw aw keepRegion 'KeepBurstLength keepLock keepCache keepPermissions keepQos raData,
     M2S_ReadData)
    dat
    depth
    = (BitVector (Width aw), Maybe (BitVector (Width aw)), rdUser, rdUser, rdUser)
    -- data address, status address, user responses for: fifo item, fifo status, error

  fifoOtpS0 _ _ _ =
    (errorX "FifoOutput for Axi4: No initial value for status vs data",
     0,
     errorX "FifoOutput for Axi4: No initial value for read id",
     S2M_NoReadData)

  fifoOtpBlank _ _ _ = (S2M_ReadAddress { _arready = False }, S2M_NoReadData)

  fifoOtpFn _ _ (dataAddr,statusAddr,usrA,usrB,usrC) (addrVal, dataAck) amtLeft queueItem = do
    addrAck <- processAddr addrVal
    (dataVal,popped) <- sendData
    processDataAck dataAck
    pure ((addrAck,dataVal),popped)
    where
      processAddr M2S_NoReadAddress = pure (S2M_ReadAddress { _arready = False })
      processAddr M2S_ReadAddress{_arburst} | _arburst /= BmFixed = pure (S2M_ReadAddress{ _arready = True })
      processAddr M2S_ReadAddress{_araddr,_arlen,_arid} = do
        (_,burstLenLeft,_,d) <- get
        when (burstLenLeft == 0 && (_araddr == dataAddr || Just _araddr == statusAddr)) $ put (_araddr == dataAddr, _arlen, _arid, d)
        pure (S2M_ReadAddress{ _arready = burstLenLeft == 0 })

      sendData = do
        (isData,burstLenLeft,readId,currOtp) <- get
        popped <- case (currOtp, isData, burstLenLeft == 0, amtLeft == maxBound) of
          (S2M_NoReadData, True, False, False) -> do
            put (isData, burstLenLeft-1, readId, S2M_ReadData { _rid = readId, _rdata = Right queueItem, _rresp = ROkay, _rlast = burstLenLeft == 1, _ruser = usrA })
            pure True
          (S2M_NoReadData, True, False, True) -> do
            put (isData, burstLenLeft-1, readId, S2M_ReadData { _rid = readId, _rdata = Left amtLeft, _rresp = RSlaveError, _rlast = burstLenLeft == 1, _ruser = usrC })
            pure False
          (S2M_NoReadData, False, False, _) -> do
            put (isData, burstLenLeft-1, readId, S2M_ReadData { _rid = readId, _rdata = Left amtLeft, _rresp = ROkay, _rlast = burstLenLeft == 1, _ruser = usrB })
            pure False
          _ -> pure False
        (_,_,_,currOtp') <- get
        pure (currOtp', popped)

      processDataAck M2S_ReadData{_rready} = when _rready $ do
        (a,b,c,_) <- get
        put (a,b,c,S2M_NoReadData)
