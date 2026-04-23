{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{- |
Experimental 'DfConv' support.

Import this module instead of 'Protocols.DfConv' when working with
experimental protocols that rely on experimental 'DfConv' instances.
-}
module Protocols.Experimental.DfConv (
  module Protocols.DfConv,
) where

import Clash.Prelude
import Control.Monad (when)
import Control.Monad.State (get, put, runState)
import Data.Maybe (isJust)
import Prelude qualified as P

import Protocols
import Protocols.DfConv
import Protocols.Experimental.Axi4.Common
import Protocols.Experimental.Axi4.ReadAddress
import Protocols.Experimental.Axi4.ReadData
import Protocols.Experimental.Axi4.WriteAddress
import Protocols.Experimental.Axi4.WriteData
import Protocols.Experimental.Axi4.WriteResponse
import Protocols.Internal

-- Manager end (toDfCircuit) only allows for burst length of 1, will ignore the
-- burst length you input. Subordinate end (fromDfCircuit) allows for any burst
-- length.
instance
  ( KnownAxi4WriteAddressConfig confAW
  , KnownAxi4WriteDataConfig confW
  , KnownAxi4WriteResponseConfig confB
  , NFDataX userAW
  , NFDataX userB
  , AWIdWidth confAW ~ BIdWidth confB
  ) =>
  DfConv
    ( Axi4WriteAddress dom confAW userAW
    , Axi4WriteData dom confW userW
    , Reverse (Axi4WriteResponse dom confB userB)
    )
  where
  type
    Dom
      ( Axi4WriteAddress dom confAW userAW
      , Axi4WriteData dom confW userW
      , Reverse (Axi4WriteResponse dom confB userB)
      ) =
      dom

  type
    FwdPayload
      ( Axi4WriteAddress dom confAW userAW
      , Axi4WriteData dom confW userW
      , Reverse (Axi4WriteResponse dom confB userB)
      ) =
      ( Axi4WriteAddressInfo confAW userAW
      , BurstLengthType (AWKeepBurstLength confAW)
      , BurstType (AWKeepBurst confAW)
      , StrictStrobeType (WNBytes confW) (WKeepStrobe confW)
      , userW
      )

  type
    BwdPayload
      ( Axi4WriteAddress dom confAW userAW
      , Axi4WriteData dom confW userW
      , Reverse (Axi4WriteResponse dom confB userB)
      ) =
      (ResponseType (BKeepResponse confB), userB)

  toDfCircuit proxy = toDfCircuitHelper proxy s0 blankOtp stateFn
   where
    s0 = (False, False)
    blankOtp =
      ( M2S_NoWriteAddress
      , M2S_NoWriteData
      , M2S_WriteResponse{_bready = False}
      )

    stateFn (addrAck, dataAck, respVal) dfAckIn dfDatIn = do
      st0 <- get
      let
        (addrMsg, st1) = runState (sendAddr addrAck dfDatIn) st0
        ((dataMsg, dfAckOut), st2) = runState (sendData dataAck dfDatIn) st1
        ((respAck, dfDatOut), st3) = runState (receiveResp respVal dfAckIn) st2
      put st3
      P.pure ((addrMsg, dataMsg, respAck), dfDatOut, dfAckOut)

    sendAddr _ Nothing = P.pure M2S_NoWriteAddress
    sendAddr addrAck (Just (info, _, burst, _, _)) = do
      (addrReceived, b) <- get
      put (addrReceived || _awready addrAck, b)
      P.pure
        $ if addrReceived
          then M2S_NoWriteAddress
          else
            axi4WriteAddrMsgFromWriteAddrInfo
              (toKeepType 0)
              burst
              info

    sendData _ Nothing = P.pure (M2S_NoWriteData, False)
    sendData dataAck (Just (_, _, _, dat, user)) = do
      (addrReceived, dataReceived) <- get
      put (addrReceived, dataReceived || _wready dataAck)
      P.pure
        $ if not addrReceived || dataReceived
          then (M2S_NoWriteData, False)
          else
            ( M2S_WriteData
                { _wdata = dat
                , _wlast = True
                , _wuser = user
                }
            , _wready dataAck
            )

    receiveResp S2M_NoWriteResponse _ =
      P.pure (M2S_WriteResponse{_bready = False}, Nothing)
    receiveResp S2M_WriteResponse{_bresp, _buser} dfAckIn = do
      (_, dataReceived) <- get
      let shouldAckResponse = dataReceived && dfAckIn
      when shouldAckResponse $ put (False, False)
      P.pure
        ( M2S_WriteResponse{_bready = shouldAckResponse}
        , Just (_bresp, _buser)
        )

  fromDfCircuit proxy = fromDfCircuitHelper proxy s0 blankOtp stateFn
   where
    s0 = (Nothing, Nothing)
    blankOtp =
      ( S2M_WriteAddress{_awready = False}
      , S2M_WriteData{_wready = False}
      , S2M_NoWriteResponse
      )

    stateFn (addrVal, dataVal, respAck) dfAckIn dfDatIn = do
      st0 <- get
      let
        (addrAck, st1) = runState (processWAddr addrVal) st0
        ((dataAck, dfDatOut), st2) = runState (processWData dataVal dfAckIn) st1
        ((respVal, dfAckOut), st3) = runState (sendWResp respAck dfDatIn) st2
      put st3
      P.pure ((addrAck, dataAck, respVal), dfDatOut, dfAckOut)

    processWAddr M2S_NoWriteAddress = P.pure (S2M_WriteAddress{_awready = False})
    processWAddr msg = do
      (writingInfo, b) <- get
      put
        ( writingInfo
            <|> Just (axi4WriteAddrMsgToWriteAddrInfo msg, _awlen msg, _awburst msg)
        , b
        )
      P.pure (S2M_WriteAddress{_awready = True})

    processWData M2S_NoWriteData _ = P.pure (S2M_WriteData{_wready = False}, Nothing)
    processWData M2S_WriteData{_wlast, _wdata, _wuser} dfAckIn = do
      (writingInfo, respID) <- get
      case writingInfo of
        Nothing -> P.pure (S2M_WriteData{_wready = False}, Nothing)
        Just (info, len, burst) -> do
          when dfAckIn
            $ put
              ( Nothing
              , if _wlast
                  then Just (_awiid info)
                  else respID
              )
          P.pure
            ( S2M_WriteData{_wready = dfAckIn}
            , Just (info, len, burst, _wdata, _wuser)
            )

    sendWResp respAck dfDatIn = do
      (a, respID) <- get
      let (respVal, dfAckOut) = case (respID, dfDatIn) of
            (Just _bid, Just (_bresp, _buser)) ->
              (S2M_WriteResponse{..}, _bready respAck)
            _ -> (S2M_NoWriteResponse, False)
      when dfAckOut $ put (a, Nothing)
      P.pure (respVal, dfAckOut)

instance
  ( KnownAxi4ReadAddressConfig confAR
  , KnownAxi4ReadDataConfig confR
  , NFDataX userR
  , NFDataX dat
  , ARIdWidth confAR ~ RIdWidth confR
  ) =>
  DfConv
    ( Axi4ReadAddress dom confAR dataAR
    , Reverse (Axi4ReadData dom confR userR dat)
    )
  where
  type
    Dom
      ( Axi4ReadAddress dom confAR dataAR
      , Reverse (Axi4ReadData dom confR userR dat)
      ) =
      dom

  type
    BwdPayload
      ( Axi4ReadAddress dom confAR dataAR
      , Reverse (Axi4ReadData dom confR userR dat)
      ) =
      (dat, userR, ResponseType (RKeepResponse confR))

  type
    FwdPayload
      ( Axi4ReadAddress dom confAR dataAR
      , Reverse (Axi4ReadData dom confR userR dat)
      ) =
      Axi4ReadAddressInfo confAR dataAR

  toDfCircuit proxy = toDfCircuitHelper proxy s0 blankOtp stateFn
   where
    s0 = ()
    blankOtp =
      ( M2S_NoReadAddress
      , M2S_ReadData{_rready = False}
      )

    stateFn (addrAck, readVal) dfAckIn dfDatIn =
      let readAddrMsg = processAddrInfo dfDatIn
       in P.pure
            ( (readAddrMsg, M2S_ReadData{_rready = dfAckIn})
            , processReadVal readVal
            , isJust dfDatIn && getDfAckOut addrAck readAddrMsg
            )

    processAddrInfo = maybe M2S_NoReadAddress axi4ReadAddrMsgFromReadAddrInfo

    processReadVal S2M_NoReadData = Nothing
    processReadVal S2M_ReadData{..} = Just (_rdata, _ruser, _rresp)

    getDfAckOut _ M2S_NoReadAddress = False
    getDfAckOut addrAck _ = _arready addrAck

  fromDfCircuit proxy = fromDfCircuitHelper proxy s0 blankOtp stateFn
   where
    s0 = (0, errorX "DfConv for Axi4: No initial value for read id")
    blankOtp = (S2M_ReadAddress{_arready = False}, S2M_NoReadData)

    stateFn (addrVal, dataAck) dfAckIn dfDatIn = do
      st0 <- get
      let
        ((addrAck, dfDatOut), st1) = runState (processAddr addrVal dfAckIn) st0
        ((dataVal, dfAckOut), st2) = runState (sendData dfDatIn dataAck) st1
      put st2
      P.pure ((addrAck, dataVal), dfDatOut, dfAckOut)

    processAddr M2S_NoReadAddress _ =
      P.pure (S2M_ReadAddress{_arready = False}, Nothing)
    processAddr msg dfAckIn = do
      (burstLenLeft, _) <- get
      case burstLenLeft of
        0 -> do
          put (succResizing $ fromKeepTypeDef 0 $ _arlen msg, _arid msg)
          P.pure
            ( S2M_ReadAddress{_arready = dfAckIn}
            , Just (axi4ReadAddrMsgToReadAddrInfo msg)
            )
        _ ->
          P.pure
            ( S2M_ReadAddress{_arready = False}
            , Nothing
            )

    succResizing :: (KnownNat n) => Index n -> Index (n + 1)
    succResizing n = resize n + 1

    sendData dfDatIn dataAck = do
      (burstLenLeft, readId) <- get
      case (burstLenLeft == 0, dfDatIn) of
        (False, Just (_rdata, _ruser, _rresp)) -> do
          put (burstLenLeft - 1, readId)
          P.pure
            ( S2M_ReadData
                { _rid = readId
                , _rlast = burstLenLeft == 1
                , _rdata
                , _ruser
                , _rresp
                }
            , _rready dataAck
            )
        _ -> P.pure (S2M_NoReadData, False)
