{-|
This module implements a type class 'DfConv' which serves as a generalization
of the 'Protocols.Df.Df' protocol. Similar protocols can provide an instance
for it and subsequently expose all the functionality implemented in this module.
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
-- UndecidableInstances is for AXI4 instances, which use a tuple of
-- (WriteAddr, WriteData, WriteResponse) or
-- (ReadAddr, ReadData)

module Protocols.DfConv
  (
    -- * Typeclass and its associated types/fns
    DfConv
  , Dom
  , BwdPayload
  , FwdPayload
  , toDfCircuit
  , fromDfCircuit

    -- * Helper functions
  , toDfCircuitHelper
  , fromDfCircuitHelper
  , dfToDfConvInp
  , dfToDfConvOtp
  , vecToDfConv
  , vecFromDfConv
  , tupToDfConv
  , tupFromDfConv

    -- * Df functions generalized to Dflike
  , convert
  , const, void, pure
  , map, bimap
  , fst, snd
  , mapMaybe, catMaybes
  , filter
  , either
  , first, {-firstT,-} mapLeft
  , second, {-secondT,-} mapRight
  , zipWith, zip
  , partition
  , route
  , select
  , selectN
  , selectUntil
  , fanin
  , mfanin
  , fanout
  , bundleVec
  , unbundleVec
  , roundrobin
  , Df.CollectMode(..)
  , roundrobinCollect
  , registerFwd
  , registerBwd
  , fifo

    -- * Simulation functions
  , drive
  , sample
  , stall
  , simulate
  ) where


import qualified Prelude as P
import           Control.Arrow ((***))
import           Control.Monad (when)
import           Control.Monad.State (State, runState, get, put)
import           Clash.Prelude hiding
                   (map, fst, snd, zipWith, const, pure, filter, either, zip, select, sample, simulate)
import qualified Clash.Prelude as C
import qualified Data.Bifunctor as B
import           Data.Maybe (fromMaybe, isNothing)
import           Data.Proxy (Proxy(..))
import           Data.Tuple (swap)
import           GHC.Stack (HasCallStack)

-- me
import           Protocols.Axi4.Common
import           Protocols.Axi4.ReadAddress
import           Protocols.Axi4.ReadData
import           Protocols.Axi4.WriteAddress
import           Protocols.Axi4.WriteData
import           Protocols.Axi4.WriteResponse
import           Protocols.Df (Data(..), Df)
import qualified Protocols.Df as Df
import           Protocols.Internal


-- | Class for protocols that are "similar" to 'Df', i.e. they can be
-- converted into a 'Df' port using a 'Circuit' (see 'toDfCircuit'). This is
-- for protocols that carry some "interesting" data, as well as some
-- "uninteresting" data (e.g. address, burst length). The 'Circuit' should
-- abstract away the complexities of each protocol, so that they can be dealt
-- with uniformly using 'Df'. For pipelined protocols, which can carry both in
-- the same cycle, the 'Circuit' should pass along the interesting parts but
-- not the uninteresting parts.
--
-- Can take parameters, e.g. for addresses. Defaults to being the right side
-- of the circuit (@Fwd ~ Otp, Bwd ~ Inp@), but this can be switched using
-- 'Reverse' from 'Protocols.Internal'. Supports both bwd (input/read) data
-- and fwd (output/write) data.
class (Protocol df) => DfConv df where
  -- | Domain that messages are being sent over. It should be true that
  -- @Fwd df ~ Signal dom [something]@ and that @Bwd df ~ Signal dom [something]@.
  type Dom df :: Domain
  -- | Information being sent into the port, along @Bwd df@. This is the
  -- information being carried over the protocol, /not/ the messages being
  -- carried over the protocol, so it doesn't include auxiliary information
  -- like address or burst length. If no data is sent in this direction, set
  -- this to @()@.
  type BwdPayload df
  -- | Information being sent out from the port, along @Fwd df@. This is the
  -- information being carried over the protocol, /not/ the messages being
  -- carried over the protocol, so it doesn't include auxiliary information like
  -- address or burst length. If no data is sent in this direction, set this to @()@.
  type FwdPayload df
  -- | Circuit which converts Df into this protocol's messages. This should
  -- deal with all the complexities of your protocol such as addresses, bursts,
  -- pipelining, etc. so that a circuit connected to the 'Df' end doesn't have
  -- to worry about all that. There are two Df channels, one for fwd data and
  -- one for bwd data, so data can be sent both ways at once. This circuit is
  -- expected to follow all of the conventions of 'Df'; for example, 'Df.Data'
  -- should stay the same between clock cycles unless acknowledged.
  toDfCircuit :: (HiddenClockResetEnable (Dom df)) =>
    Proxy df ->
    Circuit
      (Df (Dom df) (FwdPayload df), Reverse (Df (Dom df) (BwdPayload df)))
      df
  -- | 'toDfCircuit', but in reverse: the @df@ port is on the left side instead
  -- of the right side.
  fromDfCircuit :: (HiddenClockResetEnable (Dom df)) =>
    Proxy df ->
    Circuit
      df
      (Df (Dom df) (FwdPayload df), Reverse (Df (Dom df) (BwdPayload df)))

  -- defaults
  type BwdPayload df = ()
  type FwdPayload df = ()

-- | Helper function to make it easier to implement 'toDfCircuit' in 'DfConv'.
-- 'Ack's are automatically converted to/from 'Bool's, and 'Df.Data's to/from
-- 'Maybe'. A default @otpMsg@ value is given for if reset is currently on. The
-- 'State' machine is run every clock cycle. Parameters: initial state, default
-- @otpMsg@, and 'State' machine function
toDfCircuitHelper ::
  ( HiddenClockResetEnable dom
  , Protocol df
  , Bwd df ~ Unbundled dom inpMsg
  , Fwd df ~ Unbundled dom otpMsg
  , NFDataX state
  , Bundle inpMsg
  , Bundle otpMsg ) =>
  Proxy df ->
  state ->
  otpMsg ->
  ( inpMsg ->
    Bool ->
    Maybe fwdPayload ->
    State state (otpMsg, Maybe bwdPayload, Bool)
    ) ->
  Circuit ( Df dom fwdPayload
          , Reverse (Df dom bwdPayload))
          df
toDfCircuitHelper _ s0 blankOtp stateFn
  = Circuit
  $ (unbundle *** unbundle)
  . unbundle
  . hideReset cktFn
  . bundle
  . (bundle *** bundle)
 where
  cktFn reset inp = mealy transFn s0 ((,) <$> unsafeToHighPolarity reset <*> inp)
  transFn _ (True, _) = (s0, ((Ack False, NoData), blankOtp))
  transFn s (False, ((toOtp, Ack inpAck), inp)) = let
    ((otp, inputted, otpAck), s') = runState
                                    (stateFn inp inpAck (Df.dataToMaybe toOtp))
                                    s
    in (s', ((Ack otpAck, Df.maybeToData inputted), otp))

-- | Helper function to make it easier to implement 'fromDfCircuit' in 'DfConv'.
-- 'Ack's are automatically converted to/from 'Bool's, and 'Df.Data's to/from
-- 'Maybe'. A default @otpMsg@ value is given for if reset is currently on. The
-- 'State' machine is run every clock cycle. Parameters: initial state, default
-- @otpMsg@, and 'State' machine function
fromDfCircuitHelper ::
  ( HiddenClockResetEnable dom
  , Protocol df
  , Fwd df ~ Unbundled dom inpMsg
  , Bwd df ~ Unbundled dom otpMsg
  , NFDataX state
  , Bundle inpMsg
  , Bundle otpMsg ) =>
  Proxy df ->
  state ->
  otpMsg ->
  ( inpMsg ->
    Bool ->
    Maybe bwdPayload ->
    State state (otpMsg, Maybe fwdPayload, Bool)
    ) ->
  Circuit df (Df dom fwdPayload, Reverse (Df dom bwdPayload))
fromDfCircuitHelper df s0 blankOtp stateFn
  = mapCircuit id id swap swap
  $ reverseCircuit
  $ toDfCircuitHelper (reverseProxy df) s0 blankOtp stateFn
 where
  reverseProxy :: Proxy df -> Proxy (Reverse df)
  reverseProxy Proxy = Proxy


-- Trivial DfConv instance for (Df, Reverse Df)

instance DfConv (Df dom a, Reverse (Df dom b)) where
  type Dom        (Df dom a, Reverse (Df dom b)) = dom
  type BwdPayload (Df dom a, Reverse (Df dom b)) = b
  type FwdPayload (Df dom a, Reverse (Df dom b)) = a
  toDfCircuit _ = Circuit swap
  fromDfCircuit _ = Circuit swap


-- DfConv instances for Df

instance (NFDataX dat) => DfConv (Df dom dat) where
  type Dom        (Df dom dat) = dom
  type FwdPayload (Df dom dat) = dat
  toDfCircuit _ = Circuit (uncurry f) where
    f ~(a, _) c = ((c, P.pure NoData), a)
  fromDfCircuit _ = Circuit (uncurry f) where
    f a ~(b, _) = (b, (a, P.pure (Ack False)))


-- DfConv instances for Axi4

-- Manager end only allows for burst length of 1, will ignore the burst length
-- you input
instance
  ( GoodAxi4WriteAddressConfig confAW
  , GoodAxi4WriteDataConfig confW
  , GoodAxi4WriteResponseConfig confB
  , NFDataX userAW
  , NFDataX userB
  , AWIdWidth confAW ~ BIdWidth confB ) =>
  DfConv
    (Axi4WriteAddress dom confAW userAW,
     Axi4WriteData dom confW userW,
     Reverse (Axi4WriteResponse dom confB userB))
    where

  type Dom
    (Axi4WriteAddress dom confAW userAW,
     Axi4WriteData dom confW userW,
     Reverse (Axi4WriteResponse dom confB userB))
    = dom

  type FwdPayload
    (Axi4WriteAddress dom confAW userAW,
     Axi4WriteData dom confW userW,
     Reverse (Axi4WriteResponse dom confB userB))
    = ( Axi4WriteAddressInfo confAW userAW
      , BurstLengthType (AWKeepBurstLength confAW)
      , BurstType (AWKeepBurst confAW)
      , StrictStrobeType (WNBytes confW) (WKeepStrobe confW)
      , userW)

  type BwdPayload
    (Axi4WriteAddress dom confAW userAW,
     Axi4WriteData dom confW userW,
     Reverse (Axi4WriteResponse dom confB userB))
    = (ResponseType (BKeepResponse confB), userB)

  -- MANAGER PORT

  toDfCircuit proxy = toDfCircuitHelper proxy s0 blankOtp stateFn where
    s0 = (False, False) -- address received, data received

    blankOtp =
      ( M2S_NoWriteAddress
      , M2S_NoWriteData
      , M2S_WriteResponse { _bready = False }
      )

    stateFn (addrAck, dataAck, respVal) dfAckIn dfDatIn = do
      addrMsg <- sendAddr addrAck dfDatIn
      (dataMsg, dfAckOut) <- sendData dataAck dfDatIn
      (respAck, dfDatOut) <- receiveResp respVal dfAckIn
      P.pure ((addrMsg, dataMsg, respAck), dfDatOut, dfAckOut)

    sendAddr _ Nothing = P.pure M2S_NoWriteAddress
    sendAddr S2M_WriteAddress{_awready} (Just (info, _, burst, _, _)) = do
      (addrReceived, b) <- get
      put (_awready || addrReceived, b)
      P.pure $ if addrReceived then M2S_NoWriteAddress
               else axi4WriteAddrMsgFromWriteAddrInfo
                    (toKeepType 1)
                    burst
                    info

    sendData _ Nothing = P.pure (M2S_NoWriteData, False)
    sendData S2M_WriteData{_wready} (Just (_, _, _, dat, user)) = do
      (addrReceived, dataReceived) <- get
      put (addrReceived, _wready || dataReceived)
      P.pure $ if (not addrReceived || dataReceived)
        then (M2S_NoWriteData, False)
        else
          ( M2S_WriteData
            { _wdata = dat
            , _wlast = True
            , _wuser = user
            }
          , _wready)

    receiveResp S2M_NoWriteResponse _
      = P.pure (M2S_WriteResponse { _bready = False }, Nothing)
    receiveResp S2M_WriteResponse{_bresp,_buser} dfAckIn = do
      (_, dataReceived) <- get
      let shouldAckResponse = dataReceived && dfAckIn
      when shouldAckResponse $ put (False, False)
      P.pure ( M2S_WriteResponse { _bready = shouldAckResponse }
             , Just (_bresp,_buser))

  -- SUBORDINATE PORT

  fromDfCircuit proxy = fromDfCircuitHelper proxy s0 blankOtp stateFn where
    -- (info about write address given most recently, ID to send write response to)
    s0 = (Nothing, Nothing)

    blankOtp = ( S2M_WriteAddress{_awready = False}
               , S2M_WriteData{_wready = False}
               , S2M_NoWriteResponse)

    stateFn (wAddrVal, wDataVal, wRespAck) dfAckIn dfDatIn = do
      wAddrAck <- processWAddr wAddrVal
      (wDataAck, dfDatOut) <- processWData wDataVal dfAckIn
      (wRespVal, dfAckOut) <- sendWResp wRespAck dfDatIn
      P.pure ((wAddrAck, wDataAck, wRespVal), dfDatOut, dfAckOut)

    processWAddr M2S_NoWriteAddress = P.pure (S2M_WriteAddress{_awready = False})
    processWAddr msg = do
      (writingInfo,b) <- get
      when (isNothing writingInfo) $
        put (Just (axi4WriteAddrMsgToWriteAddrInfo msg, _awlen msg, _awburst msg), b)
      P.pure (S2M_WriteAddress{_awready = True})

    processWData M2S_NoWriteData _ = P.pure (S2M_WriteData{_wready = False}, Nothing)
    processWData M2S_WriteData{_wlast, _wdata, _wuser} dfAckIn = do
      (writingInfo,respID) <- get
      case writingInfo of
        Nothing -> P.pure (S2M_WriteData{_wready = False}, Nothing)
        Just (info, len, burst) -> do
          when dfAckIn $ put (Nothing,
                              if _wlast
                              then Just (_awiid info)
                              else respID)
          P.pure ( S2M_WriteData{_wready = dfAckIn}
                 , Just (info, len, burst, _wdata, _wuser))

    sendWResp wRespAck dfDatIn = do
      (a, respID) <- get
      let (wRespVal, dfAckOut) = case (respID, dfDatIn) of
            (Just _bid, Just (_bresp, _buser))
              -> (S2M_WriteResponse{..}, _bready wRespAck)
            _ -> (S2M_NoWriteResponse, False)
      when dfAckOut $ put (a, Nothing)
      P.pure (wRespVal, dfAckOut)

instance
  ( GoodAxi4ReadAddressConfig confAR
  , GoodAxi4ReadDataConfig confR
  , NFDataX userR
  , NFDataX dat
  , ARIdWidth confAR ~ RIdWidth confR ) =>
  DfConv
    (Axi4ReadAddress dom confAR dataAR,
     Reverse (Axi4ReadData dom confR userR dat))
    where

  type Dom
    (Axi4ReadAddress dom confAR dataAR,
     Reverse (Axi4ReadData dom confR userR dat))
     = dom

  type BwdPayload
    (Axi4ReadAddress dom confAR dataAR,
     Reverse (Axi4ReadData dom confR userR dat))
     = (dat, userR, ResponseType (RKeepResponse confR))

  type FwdPayload
    (Axi4ReadAddress dom confAR dataAR,
     Reverse (Axi4ReadData dom confR userR dat))
     = Axi4ReadAddressInfo confAR dataAR

  -- MANAGER PORT

  toDfCircuit proxy = toDfCircuitHelper proxy s0 blankOtp stateFn where
    s0 = ()

    blankOtp =
      ( M2S_NoReadAddress
      , M2S_ReadData { _rready = False }
      )

    stateFn (addrAck, readVal) dfAckIn dfDatIn =
      let readAddrMsg = processAddrInfo dfDatIn
      in  P.pure ( (readAddrMsg, M2S_ReadData { _rready = dfAckIn })
                 , processReadVal readVal
                 , getDfAckOut addrAck readAddrMsg)

    processAddrInfo = maybe M2S_NoReadAddress axi4ReadAddrMsgFromReadAddrInfo

    processReadVal S2M_NoReadData = Nothing
    processReadVal S2M_ReadData{..} = Just (_rdata, _ruser, _rresp)

    getDfAckOut _ M2S_NoReadAddress = False
    getDfAckOut addrAck _ = _arready addrAck

  -- SUBORDINATE PORT

  fromDfCircuit proxy = fromDfCircuitHelper proxy s0 blankOtp stateFn where
    -- ( burst len left in transfer , read id currently replying to )
    s0 = (0, errorX "DfConv for Axi4: No initial value for read id" )

    blankOtp = (S2M_ReadAddress { _arready = False }, S2M_NoReadData)

    stateFn (addrVal, dataAck) dfAckIn dfDatIn = do
      (addrAck, dfDatOut) <- processAddr addrVal dfAckIn
      (dataVal, dfAckOut) <- sendData dfDatIn dataAck
      P.pure ((addrAck,dataVal),dfDatOut,dfAckOut)

    processAddr M2S_NoReadAddress _
      = P.pure (S2M_ReadAddress { _arready = False }, Nothing)
    processAddr msg dfAckIn = do
      (burstLenLeft,_) <- get
      when (burstLenLeft == 0)
        $ put (succResizing $ fromMaybe 0 (fromKeepType $ _arlen msg), _arid msg)
      P.pure (S2M_ReadAddress{ _arready = burstLenLeft == 0 && dfAckIn }
             , Just (axi4ReadAddrMsgToReadAddrInfo msg))

    succResizing :: (KnownNat n) => Index n -> Index (n+1)
    succResizing n = (resize n) + 1

    sendData dfDatIn dataAck = do
      (burstLenLeft,readId) <- get
      case (burstLenLeft == 0, dfDatIn) of
        (False, Just (_rdata, _ruser, _rresp)) -> do
          put (burstLenLeft-1, readId)
          P.pure
            ( S2M_ReadData
              { _rid = readId
              , _rlast = burstLenLeft == 1
              , _rdata, _ruser, _rresp }
            , _rready dataAck)
        _ -> P.pure (S2M_NoReadData, False)


-- | Convert 'DfConv' into a /one-way/ 'Df' port, at the data input end
dfToDfConvInp ::
  ( DfConv df
  , HiddenClockResetEnable (Dom df) ) =>
  Proxy df ->
  Circuit df (Df (Dom df) (FwdPayload df))
dfToDfConvInp = mapCircuit id id P.fst (, P.pure NoData) . fromDfCircuit

-- | Convert 'DfConv' into a /one-way/ 'Df' port, at the data output end
dfToDfConvOtp ::
  ( DfConv df
  , HiddenClockResetEnable (Dom df) ) =>
  Proxy df ->
  Circuit (Df (Dom df) (FwdPayload df)) df
dfToDfConvOtp = mapCircuit (, P.pure (Ack False)) P.fst id id . toDfCircuit

-- 'toDfCircuit', but the 'DfConv' is inside a 'Vec'
vecToDfConv ::
  DfConv df =>
  HiddenClockResetEnable (Dom df) =>
  KnownNat n =>
  Proxy df ->
  Circuit ( Vec n (Df (Dom df) (FwdPayload df))
          , Vec n (Reverse (Df (Dom df) (BwdPayload df))))
          (Vec n df)
vecToDfConv proxy = mapCircuit (uncurry C.zip) unzip id id $ vecCircuits
                  $ repeat $ toDfCircuit proxy

-- 'fromDfCircuit', but the 'DfConv' is inside a 'Vec'
vecFromDfConv ::
  DfConv df =>
  HiddenClockResetEnable (Dom df) =>
  KnownNat n =>
  Proxy df ->
  Circuit ( Vec n df )
          ( Vec n (Df (Dom df) (FwdPayload df))
          , Vec n (Reverse (Df (Dom df) (BwdPayload df))))
vecFromDfConv proxy = mapCircuit id id unzip (uncurry C.zip) $ vecCircuits
                    $ repeat $ fromDfCircuit proxy

-- 'toDfCircuit', but on a pair of 'DfConv's
tupToDfConv ::
  ( DfConv dfA
  , DfConv dfB ) =>
  Dom dfA ~ Dom dfB =>
  HiddenClockResetEnable (Dom dfA) =>
  (Proxy dfA, Proxy dfB) ->
  Circuit
    ( ( Df (Dom dfA) (FwdPayload dfA)
      , Df (Dom dfB) (FwdPayload dfB) )
    , ( Reverse (Df (Dom dfA) (BwdPayload dfA))
      , Reverse (Df (Dom dfB) (BwdPayload dfB)) )
    )
    (dfA, dfB)
tupToDfConv (argsA, argsB)
  = mapCircuit f f id id
  $ tupCircuits (toDfCircuit argsA) (toDfCircuit argsB)
  where
  f ((a,b),(c,d)) = ((a,c),(b,d))

-- 'fromDfCircuit', but on a pair of 'DfConv's
tupFromDfConv ::
  ( DfConv dfA
  , DfConv dfB ) =>
  Dom dfA ~ Dom dfB =>
  HiddenClockResetEnable (Dom dfA) =>
  (Proxy dfA, Proxy dfB) ->
  Circuit
    (dfA, dfB)
    ( ( Df (Dom dfA) (FwdPayload dfA)
      , Df (Dom dfB) (FwdPayload dfB) )
    , ( Reverse (Df (Dom dfA) (BwdPayload dfA))
      , Reverse (Df (Dom dfB) (BwdPayload dfB)) )
    )
tupFromDfConv (argsA, argsB)
  = mapCircuit id id f f
  $ tupCircuits (fromDfCircuit argsA) (fromDfCircuit argsB)
  where
  f ((a,b),(c,d)) = ((a,c),(b,d))

-- | Circuit converting a pair of items into a @Vec 2@
tupToVec :: Circuit (a,a) (Vec 2 a)
tupToVec = Circuit ((f *** g) . swap) where
  f :: Vec 2 p -> (p,p)
  f (x :> y :> Nil) = (x,y)
  f _ = undefined -- to suppress warning
  g (x,y) = x :> y :> Nil

-- | Circuit converting a @Vec 2@ into a pair of items
vecToTup :: Circuit (Vec 2 a) (a,a)
vecToTup = Circuit ((g *** f) . swap) where
  f :: Vec 2 p -> (p,p)
  f (x :> y :> Nil) = (x,y)
  f _ = undefined -- to suppress warning
  g (x,y) = x :> y :> Nil

-- | Preserves 'Df' data unmodified. Potentially useful for converting between
-- protocols.
convert ::
  ( DfConv dfA
  , DfConv dfB
  , FwdPayload dfA ~ FwdPayload dfB
  , BwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA) ) =>
  Proxy dfA ->
  Proxy dfB ->
  Circuit dfA dfB
convert dfA dfB
  =  fromDfCircuit dfA
  |> toDfCircuit dfB

-- | Like 'P.map'
map ::
  ( DfConv dfA
  , DfConv dfB
  , BwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA) ) =>
  Proxy dfA ->
  Proxy dfB ->
  (FwdPayload dfA -> FwdPayload dfB) ->
  Circuit dfA dfB
map dfA dfB f
  =  fromDfCircuit dfA
  |> tupCircuits (Df.map f) idC
  |> toDfCircuit dfB

-- | Like 'P.fst'
fst ::
  ( DfConv dfA
  , DfConv dfB
  , BwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , FwdPayload dfA ~ (a, b)
  , FwdPayload dfB ~ a ) =>
  Proxy dfA ->
  Proxy dfB ->
  Circuit dfA dfB
fst dfA dfB
  =  fromDfCircuit dfA
  |> tupCircuits Df.fst idC
  |> toDfCircuit dfB

-- | Like 'P.fst'
snd ::
  ( DfConv dfA
  , DfConv dfB
  , BwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , FwdPayload dfA ~ (a, b)
  , FwdPayload dfB ~ b ) =>
  Proxy dfA ->
  Proxy dfB ->
  Circuit dfA dfB
snd dfA dfB
  =  fromDfCircuit dfA
  |> tupCircuits Df.snd idC
  |> toDfCircuit dfB

-- | Like 'Data.Bifunctor.bimap'
bimap ::
  ( DfConv dfA
  , DfConv dfB
  , BwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , B.Bifunctor p
  , FwdPayload dfA ~ p a c
  , FwdPayload dfB ~ p b d ) =>
  Proxy dfA ->
  Proxy dfB ->
  (a -> b) ->
  (c -> d) ->
  Circuit dfA dfB
bimap dfA dfB f g
  =  fromDfCircuit dfA
  |> tupCircuits (Df.bimap f g) idC
  |> toDfCircuit dfB

-- | Like 'Data.Bifunctor.first'
first ::
  ( DfConv dfA
  , DfConv dfB
  , BwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , B.Bifunctor p
  , FwdPayload dfA ~ p a c
  , FwdPayload dfB ~ p b c ) =>
  Proxy dfA ->
  Proxy dfB ->
  (a -> b) ->
  Circuit dfA dfB
first dfA dfB f
  =  fromDfCircuit dfA
  |> tupCircuits (Df.first f) idC
  |> toDfCircuit dfB

-- | Like 'Data.Bifunctor.second'
second ::
  ( DfConv dfA
  , DfConv dfB
  , BwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , B.Bifunctor p
  , FwdPayload dfA ~ p a b
  , FwdPayload dfB ~ p a c ) =>
  Proxy dfA ->
  Proxy dfB ->
  (b -> c) ->
  Circuit dfA dfB
second dfA dfB f
  =  fromDfCircuit dfA
  |> tupCircuits (Df.second f) idC
  |> toDfCircuit dfB

-- | Acknowledge but ignore data from LHS protocol. Send a static value /b/.
const ::
  ( DfConv dfA
  , DfConv dfB
  , BwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA) ) =>
  Proxy dfA ->
  Proxy dfB ->
  FwdPayload dfB ->
  Circuit dfA dfB
const dfA dfB b
  =  fromDfCircuit dfA
  |> tupCircuits (Df.const b) idC
  |> toDfCircuit dfB

-- | Drive a constant value composed of /a/.
pure ::
  ( DfConv df
  , HiddenClockResetEnable (Dom df) ) =>
  Proxy df ->
  FwdPayload df ->
  Circuit () df
pure df a
  =  Df.pure a
  |> dfToDfConvOtp df

-- | Ignore incoming data
void ::
  ( DfConv df
  , HiddenClockResetEnable (Dom df) ) =>
  Proxy df ->
  Circuit df ()
void df = dfToDfConvInp df
  |> Df.void

-- | Like 'Data.Maybe.catMaybes'
catMaybes ::
  ( DfConv dfA
  , DfConv dfB
  , BwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , FwdPayload dfA ~ Maybe (FwdPayload dfB) ) =>
  Proxy dfA ->
  Proxy dfB ->
  Circuit dfA dfB
catMaybes dfA dfB
  =  fromDfCircuit dfA
  |> tupCircuits (Df.catMaybes) idC
  |> toDfCircuit dfB

-- | Like 'Data.Maybe.mapMaybe'
mapMaybe ::
  ( DfConv dfA
  , DfConv dfB
  , BwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , NFDataX (FwdPayload dfB) ) =>
  Proxy dfA ->
  Proxy dfB ->
  (FwdPayload dfA -> Maybe (FwdPayload dfB)) ->
  Circuit dfA dfB
mapMaybe dfA dfB f
  =  fromDfCircuit dfA
  |> tupCircuits (Df.mapMaybe f) idC
  |> toDfCircuit dfB

-- | Like 'P.filter'
filter ::
  ( DfConv dfA
  , DfConv dfB
  , BwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , FwdPayload dfA ~ FwdPayload dfB ) =>
  Proxy dfA ->
  Proxy dfB ->
  (FwdPayload dfA -> Bool) ->
  Circuit dfA dfB
filter dfA dfB f
  =  fromDfCircuit dfA
  |> tupCircuits (Df.filter f) idC
  |> toDfCircuit dfB

-- | Like 'Data.Either.Combinators.mapLeft'
mapLeft ::
  ( DfConv dfA
  , DfConv dfB
  , BwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , FwdPayload dfA ~ Either a c
  , FwdPayload dfB ~ Either b c ) =>
  Proxy dfA ->
  Proxy dfB ->
  (a -> b) ->
  Circuit dfA dfB
mapLeft dfA dfB f
  =  fromDfCircuit dfA
  |> tupCircuits (Df.mapLeft f) idC
  |> toDfCircuit dfB

-- | Like 'Data.Either.Combinators.mapRight'
mapRight ::
  ( DfConv dfA
  , DfConv dfB
  , BwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , FwdPayload dfA ~ Either a b
  , FwdPayload dfB ~ Either a c ) =>
  Proxy dfA ->
  Proxy dfB ->
  (b -> c) ->
  Circuit dfA dfB
mapRight dfA dfB f
  =  fromDfCircuit dfA
  |> tupCircuits (Df.mapRight f) idC
  |> toDfCircuit dfB

-- | Like 'Data.Either.either'
either ::
  ( DfConv dfA
  , DfConv dfB
  , BwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , FwdPayload dfA ~ Either a b ) =>
  Proxy dfA ->
  Proxy dfB ->
  (a -> FwdPayload dfB) ->
  (b -> FwdPayload dfB) ->
  Circuit dfA dfB
either dfA dfB f g
  =  fromDfCircuit dfA
  |> tupCircuits (Df.either f g) idC
  |> toDfCircuit dfB

-- | Like 'P.zipWith'. Any data not in /Payload/ is copied from stream A.
zipWith ::
  ( DfConv dfA
  , DfConv dfB
  , DfConv dfC
  , BwdPayload dfA ~ BwdPayload dfC
  , BwdPayload dfB ~ BwdPayload dfC
  , Dom dfA ~ Dom dfB
  , Dom dfA ~ Dom dfC
  , HiddenClockResetEnable (Dom dfA) ) =>
  (Proxy dfA, Proxy dfB) ->
  Proxy dfC ->
  (FwdPayload dfA -> FwdPayload dfB -> FwdPayload dfC) ->
  Circuit (dfA, dfB) dfC
zipWith dfAB dfC f
  =  tupFromDfConv dfAB
  |> coerceCircuit
  (  tupCircuits
     (Df.zipWith f)
     (reverseCircuit $ Df.roundrobin |> vecToTup))
  |> toDfCircuit dfC

-- | Like 'P.zip'
zip ::
  ( DfConv dfA
  , DfConv dfB
  , DfConv dfC
  , BwdPayload dfA ~ BwdPayload dfC
  , BwdPayload dfB ~ BwdPayload dfC
  , Dom dfA ~ Dom dfB
  , Dom dfA ~ Dom dfC
  , HiddenClockResetEnable (Dom dfA)
  , FwdPayload dfC ~ (FwdPayload dfA, FwdPayload dfB) ) =>
  (Proxy dfA, Proxy dfB) ->
  Proxy dfC ->
  Circuit (dfA, dfB) dfC
zip dfAB dfC
  =  tupFromDfConv dfAB
  |> coerceCircuit
  (  tupCircuits
     Df.zip
     (reverseCircuit $ Df.roundrobin |> vecToTup))
  |> toDfCircuit dfC

-- | Like 'P.partition'
partition ::
  ( DfConv dfA
  , DfConv dfB
  , DfConv dfC
  , BwdPayload dfA ~ BwdPayload dfB
  , BwdPayload dfA ~ BwdPayload dfC
  , Dom dfA ~ Dom dfB
  , Dom dfA ~ Dom dfC
  , HiddenClockResetEnable (Dom dfA)
  , FwdPayload dfA ~ FwdPayload dfB
  , FwdPayload dfA ~ FwdPayload dfC ) =>
  Proxy dfA ->
  (Proxy dfB, Proxy dfC) ->
  (FwdPayload dfA -> Bool) ->
  Circuit dfA (dfB, dfC)
partition dfA dfBC f
  =  fromDfCircuit dfA
  |> coerceCircuit
  (  tupCircuits
     (Df.partition f)
     (reverseCircuit $ tupToVec |> Df.roundrobinCollect Df.Parallel))
  |> tupToDfConv dfBC

-- | Route a DfConv stream to another corresponding to the index
route ::
  ( DfConv dfA
  , DfConv dfB
  , BwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , FwdPayload dfA ~ (Index n, FwdPayload dfB)
  , KnownNat n
  , 1 <= n ) =>
  Proxy dfA ->
  Proxy dfB ->
  Circuit dfA (Vec n dfB)
route dfA dfB
  =  fromDfCircuit dfA
  |> coerceCircuit
  (  tupCircuits
     Df.route
     (reverseCircuit (Df.roundrobinCollect Df.Parallel)))
  |> vecToDfConv dfB

selectHelperA :: Circuit ((a,b),(c,d)) ((a,c),(b,d))
selectHelperA = Circuit ((f *** f) . swap) where
  f ((a,b),(c,d)) = ((a,c),(b,d))

selectHelperB :: Circuit (Vec (n+1) a) (Vec n a, a)
selectHelperB = Circuit ((f *** g) . swap) where
  f :: (Vec n q, q) -> Vec (n+1) q
  f (t, h) = h :> t
  g :: Vec (n+1) q -> (Vec n q, q)
  g (h :> t) = (t, h)
  g _ = undefined -- to avoid warning

-- | Select data from the channel indicated by the DfConv stream carrying
-- @Index n@.
select ::
  ( DfConv dfA
  , DfConv dfB
  , DfConv dfC
  , BwdPayload dfA ~ BwdPayload dfC
  , BwdPayload dfB ~ BwdPayload dfC
  , Dom dfA ~ Dom dfB
  , Dom dfA ~ Dom dfC
  , HiddenClockResetEnable (Dom dfA)
  , FwdPayload dfA ~ FwdPayload dfC
  , FwdPayload dfB ~ Index n
  , KnownNat n ) =>
  (Proxy dfA, Proxy dfB) ->
  Proxy dfC ->
  Circuit (Vec n dfA, dfB) dfC
select (dfA, dfB) dfC
  =  tupCircuits (vecFromDfConv dfA) (fromDfCircuit dfB)
  |> selectHelperA
  |> coerceCircuit
  (  tupCircuits
     Df.select
     (reverseCircuit $ Df.roundrobin |> selectHelperB))
  |> toDfCircuit dfC

-- | Select /selectN/ samples from channel /n/.
selectN ::
  ( DfConv dfA
  , DfConv dfB
  , DfConv dfC
  , BwdPayload dfA ~ BwdPayload dfC
  , BwdPayload dfB ~ BwdPayload dfC
  , Dom dfA ~ Dom dfB
  , Dom dfA ~ Dom dfC
  , HiddenClockResetEnable (Dom dfA)
  , FwdPayload dfA ~ FwdPayload dfC
  , FwdPayload dfB ~ (Index n, Index selectN)
  , KnownNat n
  , KnownNat selectN ) =>
  (Proxy dfA, Proxy dfB) ->
  Proxy dfC ->
  Circuit (Vec n dfA, dfB) dfC
selectN (dfA, dfB) dfC
  =  tupCircuits (vecFromDfConv dfA) (fromDfCircuit dfB)
  |> selectHelperA
  |> coerceCircuit
  (  tupCircuits
     Df.selectN
     (reverseCircuit $ Df.roundrobin |> selectHelperB))
  |> toDfCircuit dfC

-- | Selects samples from channel /n/ until the predicate holds. The cycle in
-- which the predicate turns true is included.
selectUntil ::
  ( DfConv dfA
  , DfConv dfB
  , DfConv dfC
  , BwdPayload dfA ~ BwdPayload dfC
  , BwdPayload dfB ~ BwdPayload dfC
  , Dom dfA ~ Dom dfB
  , Dom dfA ~ Dom dfC
  , HiddenClockResetEnable (Dom dfA)
  , FwdPayload dfA ~ FwdPayload dfC
  , FwdPayload dfB ~ Index n
  , KnownNat n ) =>
  (Proxy dfA, Proxy dfB) ->
  Proxy dfC ->
  (FwdPayload dfA -> Bool) ->
  Circuit (Vec n dfA, dfB) dfC
selectUntil (dfA, dfB) dfC f
  =  tupCircuits (vecFromDfConv dfA) (fromDfCircuit dfB)
  |> selectHelperA
  |> coerceCircuit
  (  tupCircuits
     (Df.selectUntil f)
     (reverseCircuit $ Df.roundrobin |> selectHelperB))
  |> toDfCircuit dfC

-- | Copy data of a single DfConv stream to multiple. LHS will only receive an
-- acknowledgement when all RHS receivers have acknowledged data.
fanout ::
  ( DfConv dfA
  , DfConv dfB
  , BwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , FwdPayload dfA ~ FwdPayload dfB
  , NFDataX (FwdPayload dfA)
  , KnownNat numB
  , numB ~ (decNumB + 1) ) =>
  Proxy dfA ->
  Proxy dfB ->
  Circuit dfA (Vec numB dfB)
fanout dfA dfB
  =  fromDfCircuit dfA
  |> coerceCircuit
  (  tupCircuits
     Df.fanout
     (reverseCircuit (Df.roundrobinCollect Df.Parallel)))
  |> vecToDfConv dfB

-- | Merge data of multiple streams using a user supplied function
fanin ::
  ( DfConv dfA
  , DfConv dfB
  , BwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , FwdPayload dfA ~ FwdPayload dfB
  , NFDataX (FwdPayload dfA)
  , KnownNat numA
  , numA ~ (decNumA + 1) ) =>
  Proxy dfA ->
  Proxy dfB ->
  (FwdPayload dfA -> FwdPayload dfA -> FwdPayload dfA) ->
  Circuit (Vec numA dfA) dfB
fanin dfA dfB f
  =  vecFromDfConv dfA
  |> coerceCircuit
  (  tupCircuits
     (Df.fanin f)
     (reverseCircuit Df.roundrobin))
  |> toDfCircuit dfB

-- | Merge data of multiple streams using Monoid's '<>'.
mfanin ::
  ( DfConv dfA
  , DfConv dfB
  , BwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , FwdPayload dfA ~ FwdPayload dfB
  , NFDataX (FwdPayload dfA)
  , Monoid (FwdPayload dfA)
  , KnownNat numA
  , numA ~ (decNumA + 1) ) =>
  Proxy dfA ->
  Proxy dfB ->
  Circuit (Vec numA dfA) dfB
mfanin dfA dfB
  =  vecFromDfConv dfA
  |> coerceCircuit
  (  tupCircuits
     Df.mfanin
     (reverseCircuit Df.roundrobin))
  |> toDfCircuit dfB

-- | Bundle a vector of DfConv streams into one.
bundleVec ::
  ( DfConv dfA
  , DfConv dfB
  , BwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , Vec n (FwdPayload dfA) ~ FwdPayload dfB
  , KnownNat n
  , n ~ (decN + 1) ) =>
  Proxy dfA ->
  Proxy dfB ->
  Circuit (Vec n dfA) dfB
bundleVec dfA dfB
  =  vecFromDfConv dfA
  |> coerceCircuit
  (  tupCircuits
     Df.bundleVec
     (reverseCircuit Df.roundrobin))
  |> toDfCircuit dfB

-- | Split up a DfConv stream of a vector into multiple independent DfConv
-- streams.
unbundleVec ::
  ( DfConv dfA
  , DfConv dfB
  , BwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , FwdPayload dfA ~ Vec n (FwdPayload dfB)
  , NFDataX (FwdPayload dfB)
  , KnownNat n
  , 1 <= n ) =>
  Proxy dfA ->
  Proxy dfB ->
  Circuit dfA (Vec n dfB)
unbundleVec dfA dfB
  =  fromDfCircuit dfA
  |> coerceCircuit
  (  tupCircuits
     Df.unbundleVec
     (reverseCircuit (Df.roundrobinCollect Df.Parallel)))
  |> vecToDfConv dfB

-- | Distribute data across multiple components on the RHS. Useful if you want
-- to parallelize a workload across multiple (slow) workers. For optimal
-- throughput, you should make sure workers can accept data every /n/ cycles.
roundrobin ::
  ( DfConv dfA
  , DfConv dfB
  , BwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , FwdPayload dfA ~ FwdPayload dfB
  , KnownNat n
  , n ~ (decN + 1) ) =>
  Proxy dfA ->
  Proxy dfB ->
  Circuit dfA (Vec n dfB)
roundrobin dfA dfB
  =  fromDfCircuit dfA
  |> coerceCircuit
  (  tupCircuits
     Df.roundrobin
     (reverseCircuit (Df.roundrobinCollect Df.Parallel)))
  |> vecToDfConv dfB

-- | Opposite of 'roundrobin'. Useful to collect data from workers that only
-- produce a result with an interval of /n/ cycles.
roundrobinCollect ::
  ( DfConv dfA
  , DfConv dfB
  , BwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , FwdPayload dfA ~ FwdPayload dfB
  , KnownNat n
  , n ~ (decN + 1) ) =>
  Proxy dfA ->
  Proxy dfB ->
  Df.CollectMode ->
  Circuit (Vec n dfA) dfB
roundrobinCollect dfA dfB mode
  =  vecFromDfConv dfA
  |> coerceCircuit
  (  tupCircuits
     (Df.roundrobinCollect mode)
     (reverseCircuit Df.roundrobin))
  |> toDfCircuit dfB

-- | Place register on /forward/ part of a circuit.
registerFwd ::
  ( DfConv dfA
  , DfConv dfB
  , BwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , FwdPayload dfA ~ FwdPayload dfB
  , NFDataX (FwdPayload dfA) ) =>
  Proxy dfA ->
  Proxy dfB ->
  Circuit dfA dfB
registerFwd dfA dfB
  =  fromDfCircuit dfA
  |> tupCircuits (Df.registerFwd) idC
  |> toDfCircuit dfB

-- | Place register on /backward/ part of a circuit. This is implemented using
-- a in-logic two-element shift register.
registerBwd ::
  ( DfConv dfA
  , DfConv dfB
  , BwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , FwdPayload dfA ~ FwdPayload dfB
  , NFDataX (FwdPayload dfA) ) =>
  Proxy dfA ->
  Proxy dfB ->
  Circuit dfA dfB
registerBwd dfA dfB
  =  fromDfCircuit dfA
  |> tupCircuits (Df.registerBwd) idC
  |> toDfCircuit dfB

-- | A fifo buffer with user-provided depth. Uses blockram to store data
fifo ::
  ( DfConv dfA
  , DfConv dfB
  , BwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , KnownNat depth
  , 1 <= depth
  , FwdPayload dfA ~ FwdPayload dfB
  , NFDataX (FwdPayload dfA) ) =>
  Proxy dfA ->
  Proxy dfB ->
  SNat depth ->
  Circuit dfA dfB
fifo dfA dfB fifoDepth
  =  fromDfCircuit dfA
  |> tupCircuits (Df.fifo fifoDepth) idC
  |> toDfCircuit dfB where

-- | Emit values given in list. Emits no data while reset is asserted. Not
-- synthesizable.
drive ::
  ( DfConv dfA
  , HiddenClockResetEnable (Dom dfA) ) =>
  Proxy dfA ->
  SimulationConfig ->
  [Maybe (FwdPayload dfA)] ->
  Circuit () dfA
drive dfA conf s0 = Df.drive conf s0 |> dfToDfConvOtp dfA

-- | Sample protocol to a list of values. Drops values while reset is asserted.
-- Not synthesizable.
--
-- For a generalized version of 'sample', check out 'sampleC'.
sample ::
  ( DfConv dfB
  , HiddenClockResetEnable (Dom dfB) ) =>
  Proxy dfB ->
  SimulationConfig ->
  Circuit () dfB ->
  [Maybe (FwdPayload dfB)]
sample dfB conf c = Df.sample conf (c |> dfToDfConvInp dfB)

-- | Stall every valid Df packet with a given number of cycles. If there are
-- more valid packets than given numbers, passthrough all valid packets
-- without stalling. Not synthesizable.
--
-- For a generalized version of 'stall', check out 'stallC'.
stall ::
  ( DfConv dfA
  , DfConv dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , FwdPayload dfA ~ FwdPayload dfB
  , HasCallStack ) =>
  Proxy dfA ->
  Proxy dfB ->
  SimulationConfig ->
  -- | Acknowledgement to send when LHS does not send data. Stall will act
  -- transparently when reset is asserted.
  StallAck ->
  -- Number of cycles to stall for every valid Df packet
  [Int] ->
  Circuit dfA dfB
stall dfA dfB conf stallAck stalls
  =  dfToDfConvInp dfA
  |> Df.stall conf stallAck stalls
  |> dfToDfConvOtp dfB

-- | Simulate a single domain protocol. Not synthesizable.
--
-- For a generalized version of 'simulate', check out 'Protocols.simulateC'.
--
-- You may notice that things seem to be "switched around" in this function
-- compared to others (the @Circuit@ has @Reverse@ applied to its right side,
-- rather than its left, and we take the @FwdPayload@ of @dfA@ rather than
-- @dfB@). This is because we are taking a @Circuit@ as a parameter, rather
-- than returning a @Circuit@ like most other functions do.
simulate ::
  ( DfConv dfA
  , DfConv dfB
  , Dom dfA ~ Dom dfB
  , KnownDomain (Dom dfA)
  , HasCallStack ) =>
  Proxy dfA ->
  Proxy dfB ->
  -- | Simulation configuration. Use 'Data.Default.def' for sensible defaults.
  SimulationConfig ->
  -- | Circuit to simulate.
  ( Clock (Dom dfA) ->
    Reset (Dom dfA) ->
    Enable (Dom dfA) ->
    Circuit dfA dfB ) ->
  -- | Inputs
  [Maybe (FwdPayload dfA)] ->
  -- | Outputs
  [Maybe (FwdPayload dfB)]
simulate dfA dfB conf circ inputs = Df.simulate conf circ' inputs where
  circ' clk rst en
    =  withClockResetEnable clk rst en (dfToDfConvOtp dfA)
    |> circ clk rst en
    |> withClockResetEnable clk rst en (dfToDfConvInp dfB)
