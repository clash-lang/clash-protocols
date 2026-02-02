{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

-- UndecidableInstances is for AXI4 instances, which use a tuple of
-- (WriteAddr, WriteData, WriteResponse) or
-- (ReadAddr, ReadData)

{- |
This module implements a type class 'DfConv' which serves as a generalization
of the 'Protocols.Df.Df' protocol. Similar protocols can provide an instance
for it and subsequently expose all the functionality implemented in this module.
-}
module Protocols.DfConv (
  -- * Typeclass and its associated types/fns
  DfConv,
  Dom,
  BwdPayload,
  FwdPayload,
  toDfCircuit,
  fromDfCircuit,

  -- * Helper functions
  toDfCircuitHelper,
  fromDfCircuitHelper,
  dfToDfConvInp,
  dfToDfConvOtp,
  vecToDfConv,
  vecFromDfConv,
  tupToDfConv,
  tupFromDfConv,

  -- * Df functions generalized to Dflike
  convert,
  const,
  void,
  pure,
  map,
  mapS,
  bimap,
  fst,
  snd,
  mapMaybe,
  catMaybes,
  filter,
  filterS,
  either,
  first {-firstT,-},
  mapLeft,
  second {-secondT,-},
  mapRight,
  zipWith,
  zip,
  partition,
  route,
  select,
  selectN,
  selectUntil,
  fanin,
  mfanin,
  fanout,
  bundleVec,
  unbundleVec,
  roundrobin,
  Df.CollectMode (..),
  roundrobinCollect,
  registerFwd,
  registerBwd,
  fifo,

  -- * Simulation functions
  drive,
  sample,
  stall,
  simulate,
  dfConvTestBench,
  dfConvTestBenchRev,
) where

import Clash.Prelude hiding (
  const,
  either,
  filter,
  fst,
  map,
  pure,
  sample,
  select,
  simulate,
  snd,
  zip,
  zipWith,
 )
import Clash.Prelude qualified as C
import Control.Arrow ((***))
import Control.Monad (when)
import Control.Monad.State (State, get, put, runState)
import Data.Bifunctor qualified as B
import Data.Maybe (isJust)
import Data.Proxy (Proxy (..))
import Data.Tuple (swap)
import GHC.Stack (HasCallStack)
import Prelude qualified as P

-- me
import Protocols.Axi4.Common
import Protocols.Axi4.ReadAddress
import Protocols.Axi4.ReadData
import Protocols.Axi4.WriteAddress
import Protocols.Axi4.WriteData
import Protocols.Axi4.WriteResponse
import Protocols.Df (Df)
import Protocols.Df qualified as Df
import Protocols.Internal
import Protocols.Vec qualified as Vec

{- | Class for protocols that are "similar" to 'Df', i.e. they can be converted
to and from a pair of 'Df' ports (one going 'Fwd', one going 'Bwd'), using
a 'Circuit' (see 'toDfCircuit' and 'fromDfCircuit'). This is for protocols
that carry some "interesting" data, as well as some "uninteresting" data
(e.g. address, burst length). The 'Circuit' should abstract away the
complexities of each protocol, so that they can be dealt with uniformly
using 'Df'. For pipelined protocols, which can carry both in the same cycle,
the 'Circuit' should pass along the interesting parts but not the
uninteresting parts.
-}
class (Protocol df) => DfConv df where
  -- | Domain that messages are being sent over. In general, it should be true that
  -- @Fwd df ~ Signal dom [something]@ and that @Bwd df ~ Signal dom [something]@.
  type Dom df :: Domain

  -- | Information being sent in the 'Bwd' direction, along
  -- @Reverse (Df (Dom df) (BwdPayload df))@. This is the information being
  -- carried over the protocol, /not/ the messages being carried over the
  -- protocol, so it doesn't include auxiliary information like address or
  -- burst length. If no data is sent in this direction, set this to @()@.
  type BwdPayload df

  -- | Information being sent in the 'Fwd' direction, along
  -- @Df (Dom df) (FwdPayload df)@. This is the information being
  -- carried over the protocol, /not/ the messages being carried over the
  -- protocol, so it doesn't include auxiliary information like address or
  -- burst length. If no data is sent in this direction, set this to @()@.
  type FwdPayload df

  -- | Circuit which converts Df into this protocol's messages. This should
  -- deal with all the complexities of your protocol such as addresses, bursts,
  -- pipelining, etc. so that a circuit connected to the 'Df' end doesn't have
  -- to worry about all that. There are two Df channels, one for fwd data and
  -- one for bwd data, so data can be sent both ways at once. This circuit is
  -- expected to follow all of the conventions of 'Df'; for example, data on the
  -- fwd channel should stay the same between clock cycles unless acknowledged.
  toDfCircuit ::
    (HiddenClockResetEnable (Dom df)) =>
    Circuit
      (Df (Dom df) (FwdPayload df), Reverse (Df (Dom df) (BwdPayload df)))
      df

  -- | 'toDfCircuit', but in reverse: the @df@ port is on the left side instead
  -- of the right side.
  fromDfCircuit ::
    (HiddenClockResetEnable (Dom df)) =>
    Circuit
      df
      (Df (Dom df) (FwdPayload df), Reverse (Df (Dom df) (BwdPayload df)))

  -- defaults
  type BwdPayload df = ()
  type FwdPayload df = ()

{- | Helper function to make it easier to implement 'toDfCircuit' in 'DfConv'.
'Ack's are automatically converted to/from 'Bool's. A default @otpMsg@ value is
given for if reset is currently on. The 'State' machine is run every clock cycle.
Parameters: initial state, default @otpMsg@, and 'State' machine function
-}
toDfCircuitHelper ::
  ( HiddenClockResetEnable dom
  , Protocol df
  , Bwd df ~ Unbundled dom inpMsg
  , Fwd df ~ Unbundled dom otpMsg
  , NFDataX state
  , Bundle inpMsg
  , Bundle otpMsg
  ) =>
  Proxy df ->
  state ->
  otpMsg ->
  ( inpMsg ->
    Bool ->
    Maybe fwdPayload ->
    State state (otpMsg, Maybe bwdPayload, Bool)
  ) ->
  Circuit
    ( Df dom fwdPayload
    , Reverse (Df dom bwdPayload)
    )
    df
toDfCircuitHelper _ s0 blankOtp stateFn =
  Circuit Proxy Proxy
    $ (unbundle *** unbundle)
    . unbundle
    . hideReset cktFn
    . bundle
    . (bundle *** bundle)
 where
  cktFn reset inp =
    let rstLow = unsafeToActiveHigh reset
     in mealy transFn s0 ((,) <$> rstLow <*> inp)

  transFn _ (True, _) = (s0, ((Ack False, Nothing), blankOtp))
  transFn s (False, ((toOtp, Ack inpAck), inp)) =
    let
      ((otp, inputted, otpAck), s') =
        runState
          (stateFn inp inpAck toOtp)
          s
     in
      (s', ((Ack otpAck, inputted), otp))

{- | Helper function to make it easier to implement 'fromDfCircuit' in 'DfConv'.
'Ack's are automatically converted to/from 'Bool's. A default @otpMsg@ value is
given for if reset is currently on. The 'State' machine is run every clock cycle.
Parameters: initial state, default @otpMsg@, and 'State' machine function
-}
fromDfCircuitHelper ::
  ( HiddenClockResetEnable dom
  , Protocol df
  , Fwd df ~ Unbundled dom inpMsg
  , Bwd df ~ Unbundled dom otpMsg
  , NFDataX state
  , Bundle inpMsg
  , Bundle otpMsg
  ) =>
  Proxy df ->
  state ->
  otpMsg ->
  ( inpMsg ->
    Bool ->
    Maybe bwdPayload ->
    State state (otpMsg, Maybe fwdPayload, Bool)
  ) ->
  Circuit df (Df dom fwdPayload, Reverse (Df dom bwdPayload))
fromDfCircuitHelper df s0 blankOtp stateFn =
  mapCircuit id id swap swap
    $ reverseCircuit
    $ toDfCircuitHelper (reverseProxy df) s0 blankOtp stateFn
 where
  reverseProxy :: Proxy df -> Proxy (Reverse df)
  reverseProxy Proxy = Proxy

-- Trivial DfConv instance for (Df, Reverse Df)

instance DfConv (Df dom a, Reverse (Df dom b)) where
  type Dom (Df dom a, Reverse (Df dom b)) = dom
  type BwdPayload (Df dom a, Reverse (Df dom b)) = b
  type FwdPayload (Df dom a, Reverse (Df dom b)) = a
  toDfCircuit = idC
  fromDfCircuit = idC

-- DfConv instance for Reverse

instance (DfConv a) => DfConv (Reverse a) where
  type Dom (Reverse a) = Dom a
  type BwdPayload (Reverse a) = FwdPayload a
  type FwdPayload (Reverse a) = BwdPayload a
  toDfCircuit = mapCircuit swap swap id id $ reverseCircuit $ fromDfCircuit @a
  fromDfCircuit = mapCircuit id id swap swap $ reverseCircuit $ toDfCircuit @a

-- DfConv instances for Df

instance (NFDataX dat) => DfConv (Df dom dat) where
  type Dom (Df dom dat) = dom
  type FwdPayload (Df dom dat) = dat
  toDfCircuit = Circuit Proxy Proxy (uncurry f)
   where
    f ~(a, _) c = ((c, P.pure Nothing), a)
  fromDfCircuit = Circuit Proxy Proxy (uncurry f)
   where
    f a ~(b, _) = (b, (a, P.pure (Ack False)))

-- DfConv instances for Axi4

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

  -- MANAGER PORT

  toDfCircuit = toDfCircuitHelper Proxy s0 blankOtp stateFn
   where
    s0 = (False, False) -- address received, data received
    blankOtp =
      ( M2S_NoWriteAddress
      , M2S_NoWriteData
      , M2S_WriteResponse{_bready = False}
      )

    stateFn (addrAck, dataAck, respVal) dfAckIn dfDatIn = do
      st0 <- get
      let
        -- neeeded to prevent infinite loop when two axi4 circuits are connected
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

  -- SUBORDINATE PORT

  fromDfCircuit = fromDfCircuitHelper Proxy s0 blankOtp stateFn
   where
    -- (info about write address given most recently, ID to send write response to)
    s0 = (Nothing, Nothing)

    blankOtp =
      ( S2M_WriteAddress{_awready = False}
      , S2M_WriteData{_wready = False}
      , S2M_NoWriteResponse
      )

    stateFn (addrVal, dataVal, respAck) dfAckIn dfDatIn = do
      st0 <- get
      let
        -- neeeded to prevent infinite loop when two axi4 circuits are connected
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

  -- MANAGER PORT

  toDfCircuit = toDfCircuitHelper Proxy s0 blankOtp stateFn
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

  -- SUBORDINATE PORT

  fromDfCircuit = fromDfCircuitHelper Proxy s0 blankOtp stateFn
   where
    -- ( burst len left in transfer , read id currently replying to )
    s0 = (0, errorX "DfConv for Axi4: No initial value for read id")

    blankOtp = (S2M_ReadAddress{_arready = False}, S2M_NoReadData)

    stateFn (addrVal, dataAck) dfAckIn dfDatIn = do
      st0 <- get
      let
        -- neeeded to prevent infinite loop when two axi4 circuits are connected
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

-- | Convert 'DfConv' into a /one-way/ 'Df' port, at the data input end
dfToDfConvInp ::
  forall df.
  ( DfConv df
  , HiddenClockResetEnable (Dom df)
  ) =>
  Circuit df (Df (Dom df) (FwdPayload df))
dfToDfConvInp = mapCircuit id id P.fst (, P.pure Nothing) (fromDfCircuit :: Circuit df (Df (Dom df) (FwdPayload df), Reverse (Df (Dom df) (BwdPayload df))))

-- | Convert 'DfConv' into a /one-way/ 'Df' port, at the data output end
dfToDfConvOtp ::
  forall df.
  ( DfConv df
  , HiddenClockResetEnable (Dom df)
  ) =>
  Circuit (Df (Dom df) (FwdPayload df)) df
dfToDfConvOtp = mapCircuit (, P.pure (Ack False)) P.fst id id (toDfCircuit :: Circuit (Df (Dom df) (FwdPayload df), Reverse (Df (Dom df) (BwdPayload df))) df)

-- | 'toDfCircuit', but the 'DfConv' is inside a 'Vec'
vecToDfConv ::
  forall df n.
  (DfConv df) =>
  (HiddenClockResetEnable (Dom df)) =>
  (KnownNat n) =>
  Circuit
    ( Vec n (Df (Dom df) (FwdPayload df))
    , Vec n (Reverse (Df (Dom df) (BwdPayload df)))
    )
    (Vec n df)
vecToDfConv =
  mapCircuit (uncurry C.zip) unzip id id
    $ Vec.vecCircuits
    $ C.repeat
    (toDfCircuit :: Circuit (Df (Dom df) (FwdPayload df), Reverse (Df (Dom df) (BwdPayload df))) df)

-- | 'fromDfCircuit', but the 'DfConv' is inside a 'Vec'
vecFromDfConv ::
  forall df n.
  (DfConv df) =>
  (HiddenClockResetEnable (Dom df)) =>
  (KnownNat n) =>
  Circuit
    (Vec n df)
    ( Vec n (Df (Dom df) (FwdPayload df))
    , Vec n (Reverse (Df (Dom df) (BwdPayload df)))
    )
vecFromDfConv =
  mapCircuit id id unzip (uncurry C.zip)
    $ Vec.vecCircuits
    $ C.repeat
    (fromDfCircuit :: Circuit df (Df (Dom df) (FwdPayload df), Reverse (Df (Dom df) (BwdPayload df))))

-- | 'toDfCircuit', but on a pair of 'DfConv's
tupToDfConv ::
  forall dfA dfB.
  ( DfConv dfA
  , DfConv dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  ) =>
  Circuit
    ( ( Df (Dom dfA) (FwdPayload dfA)
      , Df (Dom dfB) (FwdPayload dfB)
      )
    , ( Reverse (Df (Dom dfA) (BwdPayload dfA))
      , Reverse (Df (Dom dfB) (BwdPayload dfB))
      )
    )
    (dfA, dfB)
tupToDfConv =
  mapCircuit f f id id
    $ tupCircuits (toDfCircuit :: Circuit (Df (Dom dfA) (FwdPayload dfA), Reverse (Df (Dom dfA) (BwdPayload dfA))) dfA) (toDfCircuit :: Circuit (Df (Dom dfB) (FwdPayload dfB), Reverse (Df (Dom dfB) (BwdPayload dfB))) dfB)
 where
  f ((a, b), (c, d)) = ((a, c), (b, d))

-- | 'fromDfCircuit', but on a pair of 'DfConv's
tupFromDfConv ::
  forall dfA dfB.
  ( DfConv dfA
  , DfConv dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  ) =>
  Circuit
    (dfA, dfB)
    ( ( Df (Dom dfA) (FwdPayload dfA)
      , Df (Dom dfB) (FwdPayload dfB)
      )
    , ( Reverse (Df (Dom dfA) (BwdPayload dfA))
      , Reverse (Df (Dom dfB) (BwdPayload dfB))
      )
    )
tupFromDfConv =
  mapCircuit id id f f
    $ tupCircuits (fromDfCircuit :: Circuit dfA (Df (Dom dfA) (FwdPayload dfA), Reverse (Df (Dom dfA) (BwdPayload dfA)))) (fromDfCircuit :: Circuit dfB (Df (Dom dfB) (FwdPayload dfB), Reverse (Df (Dom dfB) (BwdPayload dfB))))
 where
  f ((a, b), (c, d)) = ((a, c), (b, d))

-- | Circuit converting a pair of items into a @Vec 2@
tupToVec :: Circuit (a, a) (Vec 2 a)
tupToVec = Circuit Proxy Proxy ((f *** g) . swap)
 where
  f :: Vec 2 p -> (p, p)
  f (x :> y :> Nil) = (x, y)
  f _ = undefined -- to suppress warning
  g (x, y) = x :> y :> Nil

-- | Circuit converting a @Vec 2@ into a pair of items
vecToTup :: Circuit (Vec 2 a) (a, a)
vecToTup = Circuit Proxy Proxy ((g *** f) . swap)
 where
  f :: Vec 2 p -> (p, p)
  f (x :> y :> Nil) = (x, y)
  f _ = undefined -- to suppress warning
  g (x, y) = x :> y :> Nil

{- | Preserves 'Df' data unmodified. Potentially useful for converting between
protocols.
-}
convert ::
  forall dfA dfB.
  ( DfConv dfA
  , DfConv dfB
  , FwdPayload dfA ~ FwdPayload dfB
  , BwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  ) =>
  Circuit dfA dfB
convert =
  (fromDfCircuit :: Circuit dfA (Df (Dom dfA) (FwdPayload dfA), Reverse (Df (Dom dfA) (BwdPayload dfA))))
    |> (toDfCircuit :: Circuit (Df (Dom dfB) (FwdPayload dfB), Reverse (Df (Dom dfB) (BwdPayload dfB))) dfB)

-- | Like 'P.map'
map ::
  forall dfA dfB.
  ( DfConv dfA
  , DfConv dfB
  , BwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  ) =>
  (FwdPayload dfA -> FwdPayload dfB) ->
  Circuit dfA dfB
map f =
  (fromDfCircuit :: Circuit dfA (Df (Dom dfA) (FwdPayload dfA), Reverse (Df (Dom dfA) (BwdPayload dfA))))
    |> tupCircuits (Df.map f) idC
    |> (toDfCircuit :: Circuit (Df (Dom dfB) (FwdPayload dfB), Reverse (Df (Dom dfB) (BwdPayload dfB))) dfB)

-- | Like 'Df.mapS'
mapS ::
  forall dfA dfB.
  ( DfConv dfA
  , DfConv dfB
  , BwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  ) =>
  Signal (Dom dfA) (FwdPayload dfA -> FwdPayload dfB) ->
  Circuit dfA dfB
mapS fS =
  (fromDfCircuit :: Circuit dfA (Df (Dom dfA) (FwdPayload dfA), Reverse (Df (Dom dfA) (BwdPayload dfA))))
    |> tupCircuits (Df.mapS fS) idC
    |> (toDfCircuit :: Circuit (Df (Dom dfB) (FwdPayload dfB), Reverse (Df (Dom dfB) (BwdPayload dfB))) dfB)

-- | Like 'P.fst'
fst ::
  forall dfA dfB a b.
  ( DfConv dfA
  , DfConv dfB
  , BwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , FwdPayload dfA ~ (a, b)
  , FwdPayload dfB ~ a
  ) =>
  Circuit dfA dfB
fst =
  (fromDfCircuit :: Circuit dfA (Df (Dom dfA) (FwdPayload dfA), Reverse (Df (Dom dfA) (BwdPayload dfA))))
    |> tupCircuits Df.fst idC
    |> (toDfCircuit :: Circuit (Df (Dom dfB) (FwdPayload dfB), Reverse (Df (Dom dfB) (BwdPayload dfB))) dfB)

-- | Like 'P.fst'
snd ::
  forall dfA dfB a b.
  ( DfConv dfA
  , DfConv dfB
  , BwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , FwdPayload dfA ~ (a, b)
  , FwdPayload dfB ~ b
  ) =>
  Circuit dfA dfB
snd =
  (fromDfCircuit :: Circuit dfA (Df (Dom dfA) (FwdPayload dfA), Reverse (Df (Dom dfA) (BwdPayload dfA))))
    |> tupCircuits Df.snd idC
    |> (toDfCircuit :: Circuit (Df (Dom dfB) (FwdPayload dfB), Reverse (Df (Dom dfB) (BwdPayload dfB))) dfB)

-- | Like 'Data.Bifunctor.bimap'
bimap ::
  forall dfA dfB p a b c d.
  ( DfConv dfA
  , DfConv dfB
  , BwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , B.Bifunctor p
  , FwdPayload dfA ~ p a c
  , FwdPayload dfB ~ p b d
  ) =>
  (a -> b) ->
  (c -> d) ->
  Circuit dfA dfB
bimap f g =
  (fromDfCircuit :: Circuit dfA (Df (Dom dfA) (FwdPayload dfA), Reverse (Df (Dom dfA) (BwdPayload dfA))))
    |> tupCircuits (Df.bimap f g) idC
    |> (toDfCircuit :: Circuit (Df (Dom dfB) (FwdPayload dfB), Reverse (Df (Dom dfB) (BwdPayload dfB))) dfB)

-- | Like 'Data.Bifunctor.first'
first ::
  forall dfA dfB p a b c.
  ( DfConv dfA
  , DfConv dfB
  , BwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , B.Bifunctor p
  , FwdPayload dfA ~ p a c
  , FwdPayload dfB ~ p b c
  ) =>
  (a -> b) ->
  Circuit dfA dfB
first f =
  (fromDfCircuit :: Circuit dfA (Df (Dom dfA) (FwdPayload dfA), Reverse (Df (Dom dfA) (BwdPayload dfA))))
    |> tupCircuits (Df.first f) idC
    |> (toDfCircuit :: Circuit (Df (Dom dfB) (FwdPayload dfB), Reverse (Df (Dom dfB) (BwdPayload dfB))) dfB)

-- | Like 'Data.Bifunctor.second'
second ::
  forall dfA dfB p a b c.
  ( DfConv dfA
  , DfConv dfB
  , BwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , B.Bifunctor p
  , FwdPayload dfA ~ p a b
  , FwdPayload dfB ~ p a c
  ) =>
  (b -> c) ->
  Circuit dfA dfB
second f =
  (fromDfCircuit :: Circuit dfA (Df (Dom dfA) (FwdPayload dfA), Reverse (Df (Dom dfA) (BwdPayload dfA))))
    |> tupCircuits (Df.second f) idC
    |> (toDfCircuit :: Circuit (Df (Dom dfB) (FwdPayload dfB), Reverse (Df (Dom dfB) (BwdPayload dfB))) dfB)

-- | Acknowledge but ignore data from LHS protocol. Send a static value /b/.
const ::
  forall dfA dfB.
  ( DfConv dfA
  , DfConv dfB
  , BwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  ) =>
  FwdPayload dfB ->
  Circuit dfA dfB
const b =
  (fromDfCircuit :: Circuit dfA (Df (Dom dfA) (FwdPayload dfA), Reverse (Df (Dom dfA) (BwdPayload dfA))))
    |> tupCircuits (Df.const b) idC
    |> (toDfCircuit :: Circuit (Df (Dom dfB) (FwdPayload dfB), Reverse (Df (Dom dfB) (BwdPayload dfB))) dfB)

-- | Drive a constant value composed of /a/.
pure ::
  forall df.
  ( DfConv df
  , HiddenClockResetEnable (Dom df)
  ) =>
  FwdPayload df ->
  Circuit () df
pure a =
  Df.pure a
    |> dfToDfConvOtp

-- | Ignore incoming data
void ::
  forall df.
  ( DfConv df
  , HiddenClockResetEnable (Dom df)
  ) =>
  Circuit df ()
void =
  dfToDfConvInp
    |> Df.void

-- | Like 'Data.Maybe.catMaybes'
catMaybes ::
  forall dfA dfB.
  ( DfConv dfA
  , DfConv dfB
  , BwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , FwdPayload dfA ~ Maybe (FwdPayload dfB)
  ) =>
  Circuit dfA dfB
catMaybes =
  (fromDfCircuit :: Circuit dfA (Df (Dom dfA) (FwdPayload dfA), Reverse (Df (Dom dfA) (BwdPayload dfA))))
    |> tupCircuits Df.catMaybes idC
    |> (toDfCircuit :: Circuit (Df (Dom dfB) (FwdPayload dfB), Reverse (Df (Dom dfB) (BwdPayload dfB))) dfB)

-- | Like 'Data.Maybe.mapMaybe'
mapMaybe ::
  forall dfA dfB.
  ( DfConv dfA
  , DfConv dfB
  , BwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , NFDataX (FwdPayload dfB)
  ) =>
  (FwdPayload dfA -> Maybe (FwdPayload dfB)) ->
  Circuit dfA dfB
mapMaybe f =
  (fromDfCircuit :: Circuit dfA (Df (Dom dfA) (FwdPayload dfA), Reverse (Df (Dom dfA) (BwdPayload dfA))))
    |> tupCircuits (Df.mapMaybe f) idC
    |> (toDfCircuit :: Circuit (Df (Dom dfB) (FwdPayload dfB), Reverse (Df (Dom dfB) (BwdPayload dfB))) dfB)

-- | Like 'P.filter'
filter ::
  forall dfA dfB.
  ( DfConv dfA
  , DfConv dfB
  , BwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , FwdPayload dfA ~ FwdPayload dfB
  ) =>
  (FwdPayload dfA -> Bool) ->
  Circuit dfA dfB
filter f =
  (fromDfCircuit :: Circuit dfA (Df (Dom dfA) (FwdPayload dfA), Reverse (Df (Dom dfA) (BwdPayload dfA))))
    |> tupCircuits (Df.filter f) idC
    |> (toDfCircuit :: Circuit (Df (Dom dfB) (FwdPayload dfB), Reverse (Df (Dom dfB) (BwdPayload dfB))) dfB)

-- | Like 'Df.filterS'
filterS ::
  forall dfA dfB.
  ( DfConv dfA
  , DfConv dfB
  , BwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , FwdPayload dfA ~ FwdPayload dfB
  ) =>
  Signal (Dom dfA) (FwdPayload dfA -> Bool) ->
  Circuit dfA dfB
filterS fS =
  (fromDfCircuit :: Circuit dfA (Df (Dom dfA) (FwdPayload dfA), Reverse (Df (Dom dfA) (BwdPayload dfA))))
    |> tupCircuits (Df.filterS fS) idC
    |> (toDfCircuit :: Circuit (Df (Dom dfB) (FwdPayload dfB), Reverse (Df (Dom dfB) (BwdPayload dfB))) dfB)

-- | Like 'Data.Either.Combinators.mapLeft'
mapLeft ::
  forall dfA dfB a b c.
  ( DfConv dfA
  , DfConv dfB
  , BwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , FwdPayload dfA ~ Either a c
  , FwdPayload dfB ~ Either b c
  ) =>
  (a -> b) ->
  Circuit dfA dfB
mapLeft f =
  (fromDfCircuit :: Circuit dfA (Df (Dom dfA) (FwdPayload dfA), Reverse (Df (Dom dfA) (BwdPayload dfA))))
    |> tupCircuits (Df.mapLeft f) idC
    |> (toDfCircuit :: Circuit (Df (Dom dfB) (FwdPayload dfB), Reverse (Df (Dom dfB) (BwdPayload dfB))) dfB)

-- | Like 'Data.Either.Combinators.mapRight'
mapRight ::
  forall dfA dfB a b c.
  ( DfConv dfA
  , DfConv dfB
  , BwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , FwdPayload dfA ~ Either a b
  , FwdPayload dfB ~ Either a c
  ) =>
  (b -> c) ->
  Circuit dfA dfB
mapRight f =
  (fromDfCircuit :: Circuit dfA (Df (Dom dfA) (FwdPayload dfA), Reverse (Df (Dom dfA) (BwdPayload dfA))))
    |> tupCircuits (Df.mapRight f) idC
    |> (toDfCircuit :: Circuit (Df (Dom dfB) (FwdPayload dfB), Reverse (Df (Dom dfB) (BwdPayload dfB))) dfB)

-- | Like 'Data.Either.either'
either ::
  forall dfA dfB a b.
  ( DfConv dfA
  , DfConv dfB
  , BwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , FwdPayload dfA ~ Either a b
  ) =>
  (a -> FwdPayload dfB) ->
  (b -> FwdPayload dfB) ->
  Circuit dfA dfB
either f g =
  (fromDfCircuit :: Circuit dfA (Df (Dom dfA) (FwdPayload dfA), Reverse (Df (Dom dfA) (BwdPayload dfA))))
    |> tupCircuits (Df.either f g) idC
    |> (toDfCircuit :: Circuit (Df (Dom dfB) (FwdPayload dfB), Reverse (Df (Dom dfB) (BwdPayload dfB))) dfB)

-- | Like 'P.zipWith'. Any data not in /Payload/ is copied from stream A.
zipWith ::
  forall dfA dfB dfC.
  ( DfConv dfA
  , DfConv dfB
  , DfConv dfC
  , BwdPayload dfA ~ BwdPayload dfC
  , BwdPayload dfB ~ BwdPayload dfC
  , Dom dfA ~ Dom dfB
  , Dom dfA ~ Dom dfC
  , HiddenClockResetEnable (Dom dfA)
  ) =>
  (FwdPayload dfA -> FwdPayload dfB -> FwdPayload dfC) ->
  Circuit (dfA, dfB) dfC
zipWith f =
  tupFromDfConv
    |> coerceCircuit
      ( tupCircuits
          (Df.zipWith f)
          (reverseCircuit $ Df.roundrobin |> vecToTup)
      )
    |> (toDfCircuit :: Circuit (Df (Dom dfC) (FwdPayload dfC), Reverse (Df (Dom dfC) (BwdPayload dfC))) dfC)

-- | Like 'P.zip'
zip ::
  forall dfA dfB dfC.
  ( DfConv dfA
  , DfConv dfB
  , DfConv dfC
  , BwdPayload dfA ~ BwdPayload dfC
  , BwdPayload dfB ~ BwdPayload dfC
  , Dom dfA ~ Dom dfB
  , Dom dfA ~ Dom dfC
  , HiddenClockResetEnable (Dom dfA)
  , FwdPayload dfC ~ (FwdPayload dfA, FwdPayload dfB)
  ) =>
  Circuit (dfA, dfB) dfC
zip =
  tupFromDfConv
    |> coerceCircuit
      ( tupCircuits
          Df.zip
          (reverseCircuit $ Df.roundrobin |> vecToTup)
      )
    |> (toDfCircuit :: Circuit (Df (Dom dfC) (FwdPayload dfC), Reverse (Df (Dom dfC) (BwdPayload dfC))) dfC)

-- | Like 'P.partition'
partition ::
  forall dfA dfB dfC.
  ( DfConv dfA
  , DfConv dfB
  , DfConv dfC
  , BwdPayload dfA ~ BwdPayload dfB
  , BwdPayload dfA ~ BwdPayload dfC
  , Dom dfA ~ Dom dfB
  , Dom dfA ~ Dom dfC
  , HiddenClockResetEnable (Dom dfA)
  , FwdPayload dfA ~ FwdPayload dfB
  , FwdPayload dfA ~ FwdPayload dfC
  ) =>
  (FwdPayload dfA -> Bool) ->
  Circuit dfA (dfB, dfC)
partition f =
  (fromDfCircuit :: Circuit dfA (Df (Dom dfA) (FwdPayload dfA), Reverse (Df (Dom dfA) (BwdPayload dfA))))
    |> coerceCircuit
      ( tupCircuits
          (Df.partition f)
          (reverseCircuit $ tupToVec |> Df.roundrobinCollect Df.Parallel)
      )
    |> tupToDfConv

-- | Route a DfConv stream to another corresponding to the index
route ::
  forall dfA dfB n.
  ( DfConv dfA
  , DfConv dfB
  , BwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , FwdPayload dfA ~ (Index n, FwdPayload dfB)
  , KnownNat n
  , 1 <= n
  ) =>
  Circuit dfA (Vec n dfB)
route =
  (fromDfCircuit :: Circuit dfA (Df (Dom dfA) (FwdPayload dfA), Reverse (Df (Dom dfA) (BwdPayload dfA))))
    |> coerceCircuit
      ( tupCircuits
          Df.route
          (reverseCircuit (Df.roundrobinCollect Df.Parallel))
      )
    |> vecToDfConv

selectHelperA :: Circuit ((a, b), (c, d)) ((a, c), (b, d))
selectHelperA = Circuit Proxy Proxy ((f *** f) . swap)
 where
  f ((a, b), (c, d)) = ((a, c), (b, d))

selectHelperB :: Circuit (Vec (n + 1) a) (Vec n a, a)
selectHelperB = Circuit Proxy Proxy ((f *** g) . swap)
 where
  f :: (Vec n q, q) -> Vec (n + 1) q
  f (t, h) = h :> t
  g :: Vec (n + 1) q -> (Vec n q, q)
  g (h :> t) = (t, h)
  g _ = undefined -- to avoid warning

{- | Select data from the channel indicated by the DfConv stream carrying
@Index n@.
-}
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
  , KnownNat n
  ) =>
  Circuit (Vec n dfA, dfB) dfC
select =
  tupCircuits (vecFromDfConv) fromDfCircuit
    |> selectHelperA
    |> coerceCircuit
      ( tupCircuits
          Df.select
          (reverseCircuit $ Df.roundrobin |> selectHelperB)
      )
    |> toDfCircuit

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
  , KnownNat selectN
  ) =>
  Circuit (Vec n dfA, dfB) dfC
selectN =
  tupCircuits (vecFromDfConv) fromDfCircuit
    |> selectHelperA
    |> coerceCircuit
      ( tupCircuits
          Df.selectN
          (reverseCircuit $ Df.roundrobin |> selectHelperB)
      )
    |> toDfCircuit

{- | Selects samples from channel /n/ until the predicate holds. The cycle in
which the predicate turns true is included.
-}
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
  , KnownNat n
  ) =>
  (FwdPayload dfA -> Bool) ->
  Circuit (Vec n dfA, dfB) dfC
selectUntil f =
  tupCircuits (vecFromDfConv) fromDfCircuit
    |> selectHelperA
    |> coerceCircuit
      ( tupCircuits
          (Df.selectUntil f)
          (reverseCircuit $ Df.roundrobin |> selectHelperB)
      )
    |> toDfCircuit

{- | Copy data of a single DfConv stream to multiple. LHS will only receive an
acknowledgement when all RHS receivers have acknowledged data.
-}
fanout ::
  ( DfConv dfA
  , DfConv dfB
  , BwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , FwdPayload dfA ~ FwdPayload dfB
  , NFDataX (FwdPayload dfA)
  , KnownNat numB
  , 1 <= numB
  ) =>
  Circuit dfA (Vec numB dfB)
fanout =
  fromDfCircuit
    |> coerceCircuit
      ( tupCircuits
          Df.fanout
          (reverseCircuit (Df.roundrobinCollect Df.Parallel))
      )
    |> vecToDfConv

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
  , numA ~ (decNumA + 1)
  ) =>
  (FwdPayload dfA -> FwdPayload dfA -> FwdPayload dfA) ->
  Circuit (Vec numA dfA) dfB
fanin f =
  vecFromDfConv
    |> coerceCircuit
      ( tupCircuits
          (Df.fanin f)
          (reverseCircuit Df.roundrobin)
      )
    |> toDfCircuit

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
  , numA ~ (decNumA + 1)
  ) =>
  Circuit (Vec numA dfA) dfB
mfanin =
  vecFromDfConv
    |> coerceCircuit
      ( tupCircuits
          Df.mfanin
          (reverseCircuit Df.roundrobin)
      )
    |> toDfCircuit

-- | Bundle a vector of DfConv streams into one.
bundleVec ::
  ( DfConv dfA
  , DfConv dfB
  , BwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , Vec n (FwdPayload dfA) ~ FwdPayload dfB
  , KnownNat n
  , n ~ (decN + 1)
  ) =>
  Circuit (Vec n dfA) dfB
bundleVec =
  vecFromDfConv
    |> coerceCircuit
      ( tupCircuits
          Df.bundleVec
          (reverseCircuit Df.roundrobin)
      )
    |> toDfCircuit

{- | Split up a DfConv stream of a vector into multiple independent DfConv
streams.
-}
unbundleVec ::
  ( DfConv dfA
  , DfConv dfB
  , BwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , FwdPayload dfA ~ Vec n (FwdPayload dfB)
  , NFDataX (FwdPayload dfB)
  , KnownNat n
  , 1 <= n
  ) =>
  Circuit dfA (Vec n dfB)
unbundleVec =
  fromDfCircuit
    |> coerceCircuit
      ( tupCircuits
          Df.unbundleVec
          (reverseCircuit (Df.roundrobinCollect Df.Parallel))
      )
    |> vecToDfConv

{- | Distribute data across multiple components on the RHS. Useful if you want
to parallelize a workload across multiple (slow) workers. For optimal
throughput, you should make sure workers can accept data every /n/ cycles.
-}
roundrobin ::
  ( DfConv dfA
  , DfConv dfB
  , BwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , FwdPayload dfA ~ FwdPayload dfB
  , KnownNat n
  , n ~ (decN + 1)
  ) =>
  Circuit dfA (Vec n dfB)
roundrobin =
  fromDfCircuit
    |> coerceCircuit
      ( tupCircuits
          Df.roundrobin
          (reverseCircuit (Df.roundrobinCollect Df.Parallel))
      )
    |> vecToDfConv

{- | Opposite of 'roundrobin'. Useful to collect data from workers that only
produce a result with an interval of /n/ cycles.
-}
roundrobinCollect ::
  ( DfConv dfA
  , DfConv dfB
  , BwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , FwdPayload dfA ~ FwdPayload dfB
  , KnownNat n
  , n ~ (decN + 1)
  ) =>
  Df.CollectMode ->
  Circuit (Vec n dfA) dfB
roundrobinCollect mode =
  vecFromDfConv
    |> coerceCircuit
      ( tupCircuits
          (Df.roundrobinCollect mode)
          (reverseCircuit Df.roundrobin)
      )
    |> toDfCircuit

-- | Place register on /forward/ part of a circuit.
registerFwd ::
  ( DfConv dfA
  , DfConv dfB
  , BwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , FwdPayload dfA ~ FwdPayload dfB
  , NFDataX (FwdPayload dfA)
  ) =>
  Circuit dfA dfB
registerFwd =
  fromDfCircuit
    |> tupCircuits Df.registerFwd idC
    |> toDfCircuit

{- | Place register on /backward/ part of a circuit. This is implemented using
a in-logic two-element shift register.
-}
registerBwd ::
  ( DfConv dfA
  , DfConv dfB
  , BwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , FwdPayload dfA ~ FwdPayload dfB
  , NFDataX (FwdPayload dfA)
  ) =>
  Circuit dfA dfB
registerBwd =
  fromDfCircuit
    |> tupCircuits Df.registerBwd idC
    |> toDfCircuit

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
  , NFDataX (FwdPayload dfA)
  ) =>
  SNat depth ->
  Circuit dfA dfB
fifo fifoDepth =
  fromDfCircuit
    |> tupCircuits (Df.fifo fifoDepth) idC
    |> toDfCircuit

{- | Emit values given in list. Emits no data while reset is asserted. Not
synthesizable.
-}
drive ::
  ( DfConv dfA
  , HiddenClockResetEnable (Dom dfA)
  ) =>
  SimulationConfig ->
  [Maybe (FwdPayload dfA)] ->
  Circuit () dfA
drive conf s0 = Df.drive conf s0 |> dfToDfConvOtp

{- | Sample protocol to a list of values. Drops values while reset is asserted.
Not synthesizable.

For a generalized version of 'sample', check out 'sampleC'.
-}
sample ::
  ( DfConv dfB
  , HiddenClockResetEnable (Dom dfB)
  ) =>
  SimulationConfig ->
  Circuit () dfB ->
  [Maybe (FwdPayload dfB)]
sample conf c = Df.sample conf (c |> dfToDfConvInp)

{- | Stall every valid Df packet with a given number of cycles. If there are
more valid packets than given numbers, passthrough all valid packets
without stalling. Not synthesizable.

For a generalized version of 'stall', check out 'stallC'.
-}
stall ::
  ( DfConv dfA
  , DfConv dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , FwdPayload dfA ~ FwdPayload dfB
  , HasCallStack
  ) =>
  SimulationConfig ->
  -- | Acknowledgement to send when LHS does not send data. Stall will act
  -- transparently when reset is asserted.
  StallAck ->
  -- Number of cycles to stall for every valid Df packet
  [Int] ->
  Circuit dfA dfB
stall conf stallAck stalls =
  dfToDfConvInp
    |> Df.stall conf stallAck stalls
    |> dfToDfConvOtp

{- | Simulate a single domain protocol. Not synthesizable.

For a generalized version of 'simulate', check out 'Protocols.simulateC'.

You may notice that things seem to be "switched around" in this function
compared to others (the @Circuit@ has @Reverse@ applied to its right side,
rather than its left, and we take the @FwdPayload@ of @dfA@ rather than
@dfB@). This is because we are taking a @Circuit@ as a parameter, rather
than returning a @Circuit@ like most other functions do.
-}
simulate ::
  forall dfA dfB.
  ( DfConv dfA
  , DfConv dfB
  , Dom dfA ~ Dom dfB
  , KnownDomain (Dom dfA)
  , HasCallStack
  ) =>
  -- | Simulation configuration. Use 'Data.Default.def' for sensible defaults.
  SimulationConfig ->
  -- | Circuit to simulate.
  ( Clock (Dom dfA) ->
    Reset (Dom dfA) ->
    Enable (Dom dfA) ->
    Circuit dfA dfB
  ) ->
  -- | Inputs
  [Maybe (FwdPayload dfA)] ->
  -- | Outputs
  [Maybe (FwdPayload dfB)]
simulate conf circ = Df.simulate conf circ'
 where
  circ' clk rst en =
    withClockResetEnable clk rst en (dfToDfConvOtp)
      |> circ clk rst en
      |> withClockResetEnable clk rst en (dfToDfConvInp @dfB)

{- | Given behavior along the backward channel, turn an arbitrary 'DfConv'
circuit into an easily-testable 'Df' circuit representing the forward
channel. Behavior along the backward channel is specified by a @[Bool]@
(a list of acknowledges to provide), and a @[Maybe (BwdPayload dfB)]@ (a list
of data values to feed in).
-}
dfConvTestBench ::
  forall dfA dfB.
  ( DfConv dfA
  , DfConv dfB
  , NFDataX (BwdPayload dfB)
  , ShowX (BwdPayload dfB)
  , Show (BwdPayload dfB)
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  ) =>
  [Bool] ->
  [Maybe (BwdPayload dfB)] ->
  Circuit dfA dfB ->
  Circuit
    (Df (Dom dfA) (FwdPayload dfA))
    (Df (Dom dfB) (FwdPayload dfB))
dfConvTestBench bwdAcks bwdPayload circ =
  mapCircuit (,()) P.fst P.fst (,())
    $ tupCircuits idC (ackCircuit :: Circuit (Reverse ()) (Reverse (Df (Dom dfA) (BwdPayload dfA))))
    |> (toDfCircuit @dfA)
    |> circ
    |> (fromDfCircuit @dfB)
    |> tupCircuits idC driveCircuit
 where
  ackCircuit :: Circuit (Reverse ()) (Reverse (Df (Dom dfA) (BwdPayload dfA)))
  ackCircuit =
    reverseCircuit
      $ Circuit Proxy Proxy
      $ P.const
        ( boolsToBwd (Proxy @(Df _ _)) bwdAcks
        , ()
        )
  driveCircuit = reverseCircuit (driveC def bwdPayload)

{- | Given behavior along the forward channel, turn an arbitrary 'DfConv'
circuit into an easily-testable 'Df' circuit representing the backward
channel. Behavior along the forward channel is specified by a
@[Maybe (FwdPayload dfA)]@ (a list of data values to feed in), and a @[Bool]@
(a list of acknowledges to provide).
-}
dfConvTestBenchRev ::
  forall dfA dfB.
  ( DfConv dfA
  , DfConv dfB
  , NFDataX (FwdPayload dfA)
  , ShowX (FwdPayload dfA)
  , Show (FwdPayload dfA)
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  ) =>
  [Maybe (FwdPayload dfA)] ->
  [Bool] ->
  Circuit dfA dfB ->
  Circuit
    (Df (Dom dfB) (BwdPayload dfB))
    (Df (Dom dfA) (BwdPayload dfA))
dfConvTestBenchRev fwdPayload fwdAcks circ =
  mapCircuit ((),) P.snd P.snd ((),)
    $ reverseCircuit
    $ tupCircuits driveCircuit idC
    |> (toDfCircuit @dfA)
    |> circ
    |> (fromDfCircuit @dfB)
    |> tupCircuits (ackCircuit :: Circuit (Df (Dom dfB) (FwdPayload dfB)) ()) idC
 where
  driveCircuit = driveC def fwdPayload
  ackCircuit :: Circuit (Df (Dom dfB) (FwdPayload dfB)) ()
  ackCircuit =
    Circuit Proxy Proxy
      $ P.const
        ( boolsToBwd (Proxy @(Df _ _)) fwdAcks
        , ()
        )
