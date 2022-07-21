{-|
This module implements a type class 'DfLike' which serves as a generalization
of the 'Protocols.Df.Df' protocol. Similar protocols can provide an instance
for it and subsequently expose all the functionality implemented in this module.
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Protocols.DfLike
  (
    -- * Typeclass and its associated types/fns
    DfLike
  , Dom
  , BwdPayload
  , FwdPayload
  , DfLikeParam
  , toDfCircuit

    -- * Class and datatype for implicit info
  , ImplicitInfo(..)
  , ImplicitInfoClass
  , ImplicitInfoFwdPayload
  , ImplicitInfoBwdPayload
  , ImplicitInfoFwdParam
  , mapFwd
  , mapBwd

    -- * Helper functions
  , fromDfCircuit
  , toDfCircuitHelper

    -- * Associated with Axi4 instance
  , Axi4WriteAddressInfo
  , axi4WriteAddrMsgToWriteAddrInfo
  , axi4WriteAddrMsgFromWriteAddrInfo
  , Axi4ReadAddressInfo
  , axi4ReadAddrMsgToReadAddrInfo
  , axi4ReadAddrMsgFromReadAddrInfo

    -- * Utilities to use DfLike one-way
  , dfToDfLikeInp
  , dfToDfLikeOtp

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


-- | Class for protocols that are "similar" to 'Df',
-- i.e. they can be converted into a 'Df' port using a 'Circuit' (see 'toDfCircuit').
-- This is for protocols that carry some "interesting" data,
-- as well as some "uninteresting" data (e.g. address, burst length).
-- The 'Circuit' should abstract away the complexities of each protocol,
-- so that they can be dealt with uniformly using 'Df'.
-- For pipelined protocols, which can carry both in the same cycle,
-- the 'Circuit' should pass along the interesting parts
-- but not the uninteresting parts.
--
-- Can take parameters, e.g. for addresses.
-- Defaults to being the right side of the circuit (@Fwd ~ Otp, Bwd ~ Inp@),
-- but this can be switched using 'Reverse' from 'Protocols.Internal'.
-- Supports both bwd (input/read) data and fwd (output/write) data.
class (Protocol df) => DfLike df where
  -- | Domain that messages are being sent over.
  -- It should be true that @Fwd df ~ Signal dom [something]@
  -- and that @Bwd df ~ Signal dom [something]@.
  type Dom df :: Domain
  -- | Information being sent into the port, along @Bwd df@.
  -- This is the information being carried over the protocol,
  -- /not/ the messages being carried over the protocol,
  -- so it doesn't include auxiliary information like address or burst length.
  -- If no data is sent in this direction, set this to @()@.
  type BwdPayload df
  -- | Information being sent out from the port, along @Fwd df@.
  -- This is the information being carried over the protocol,
  -- /not/ the messages being carried over the protocol,
  -- so it doesn't include auxiliary information like address or burst length.
  -- If no data is sent in this direction, set this to @()@.
  type FwdPayload df
  -- | User-provided parameters for 'toDfCircuit'
  -- (e.g. address to respond to, so that different 'DfLike'
  -- components can respond to different addresses).
  -- If you don't need to take params, set this to @()@.
  type DfLikeParam df
  -- | Circuit which converts Df into this protocol's messages.
  -- This should deal with all the complexities of your protocol
  -- such as addresses, bursts, pipelining, etc. so that
  -- a circuit connected to the 'Df' end doesn't have to worry about all that.
  -- There are two Df channels, one for fwd data and one for bwd data,
  -- so data can be sent both ways at once.
  -- This circuit is expected to follow all of the conventions of 'Df';
  -- for example, 'Df.Data' should stay the same
  -- between clock cycles unless acknowledged.
  toDfCircuit :: (HiddenClockResetEnable (Dom df))
    => (Proxy df, DfLikeParam df)
    -> Circuit (Df (Dom df) (FwdPayload df), Reverse (Df (Dom df) (BwdPayload df))) df

  -- defaults
  type BwdPayload df = ()
  type FwdPayload df = ()
  type DfLikeParam df = ()

-- | Class for protocols whose 'DfLike' implementation contains
-- a lot of info (e.g. addresses, user channels) that typically isn't needed.
-- Using 'ImplicitInfo' we can put in default values for extraneous info
-- and only deal with the important data (see 'ImplicitInfo' and instances below).
class (DfLike df) => ImplicitInfoClass df where
  -- | The "important" parts of @BwdPayload df@
  type ImplicitInfoBwdPayload df
  -- | The "important" parts of @FwdPayload df@
  type ImplicitInfoFwdPayload df
  -- | Parameter given to 'mapFwd', typically a bunch of default values.
  type ImplicitInfoFwdParam df
  -- | Add on "unimportant" default values to 'ImplicitInfoFwdPayload' to get 'FwdPayload'
  mapFwd :: (Proxy df, ImplicitInfoFwdParam df) -> ImplicitInfoFwdPayload df -> FwdPayload df
  -- | Remove "unimportant" default values from 'BwdPayload' to get 'ImplicitInfoBwdPayload'
  mapBwd :: Proxy df -> BwdPayload df -> ImplicitInfoBwdPayload df

  -- defaults
  type ImplicitInfoBwdPayload df = ()
  type ImplicitInfoFwdPayload df = ()
  type ImplicitInfoFwdParam df = ()

-- | Wrapper for protocols whose DfLike implementation contains
-- a lot of info that typically isn't needed.
-- See 'ImplicitInfoClass' and instances.
data ImplicitInfo (t :: Type) = ImplicitInfo

-- The same data is passed forwards and backwards,
-- but we interpret it differently in 'DfLike'.
instance (Protocol df) => Protocol (ImplicitInfo df) where
  type Fwd (ImplicitInfo df) = Fwd df
  type Bwd (ImplicitInfo df) = Bwd df

-- The important part to note here is 'FwdPayload' and 'BwdPayload'.
instance (ImplicitInfoClass df) => DfLike (ImplicitInfo df) where
  type Dom (ImplicitInfo df) = Dom df
  type BwdPayload (ImplicitInfo df) = ImplicitInfoBwdPayload df
  type FwdPayload (ImplicitInfo df) = ImplicitInfoFwdPayload df
  type DfLikeParam (ImplicitInfo df) = (ImplicitInfoFwdParam df, DfLikeParam df)
  toDfCircuit (_, (fwdParam, dfParam)) =
    let pxy = Proxy :: Proxy df
    in  coerceCircuit -- we have Circuit (...) df but want Circuit (...) (ImplicitInfo df), which has same 'Fwd' and 'Bwd'
    $   tupCircuits
        (Df.map $ mapFwd (pxy, fwdParam))
        (reverseCircuit $ Df.map $ mapBwd pxy)
    |>  toDfCircuit (pxy, dfParam)

-- | 'toDfCircuit', but 'Df' is on the other side.
-- 'BwdPayload' remains the input data,
-- and 'FwdPayload' remains the output data.
-- All the functionality from 'toDfCircuit' is preserved.
fromDfCircuit
  :: (DfLike df, HiddenClockResetEnable (Dom df))
  => (Proxy df, DfLikeParam df)
  -> Circuit (Reverse df)
             (Df (Dom df) (BwdPayload df), Reverse (Df (Dom df) (FwdPayload df)))
fromDfCircuit = mapCircuit id id swap swap . reverseCircuit . toDfCircuit

-- | Helper function to make it easier to implement 'DfLike'.
-- 'Ack's are automatically converted to/from 'Bool's,
-- and 'Df.Data's to/from 'Maybe'.
-- A default @otpMsg@ value is given for if reset is currently on.
-- The 'State' machine is run every clock cycle.
-- Parameters: initial state, default @otpMsg@, and 'State' machine function
toDfCircuitHelper ::
  ( HiddenClockResetEnable (Dom df)
  , Protocol df
  , Bwd df ~ Unbundled (Dom df) inpMsg
  , Fwd df ~ Unbundled (Dom df) otpMsg
  , NFDataX state
  , Bundle inpMsg
  , Bundle otpMsg
  )
  => state
  -> otpMsg
  -> ( inpMsg
    -> Bool
    -> Maybe (FwdPayload df)
    -> State state (otpMsg, Maybe (BwdPayload df), Bool)
     )
  -> Circuit (Df (Dom df) (FwdPayload df), Reverse (Df (Dom df) (BwdPayload df))) df
toDfCircuitHelper s0 blankOtp stateFn
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
    ((otp, inputted, otpAck), s') = runState (stateFn inp inpAck (Df.dataToMaybe toOtp)) s
    in (s', ((Ack otpAck, Df.maybeToData inputted), otp))


-- DfLike classes for Df

instance (NFDataX dat) => DfLike (Reverse (Df dom dat)) where
  type Dom         (Reverse (Df dom dat)) = dom
  type BwdPayload  (Reverse (Df dom dat)) = dat
  toDfCircuit _ = Circuit (uncurry f) where
    f ~(_, b) c = ((P.pure (Ack False), c), b)

instance (NFDataX dat) => DfLike (Df dom dat) where
  type Dom         (Df dom dat) = dom
  type FwdPayload  (Df dom dat) = dat
  toDfCircuit _ = Circuit (uncurry f) where
    f ~(a, _) c = ((c, P.pure NoData), a)


-- DfLike classes for Axi4

-- | Data carried along 'Axi4WriteAddress' channel which is put in control of the user,
-- rather than being managed by the 'DfLike' instances.
-- Matches up one-to-one with the fields of 'M2S_WriteAddress' except for
-- '_awlen', '_awsize', and '_awburst'.
-- TODO where should I put this and 'Axi4ReadAddressInfo'?
data Axi4WriteAddressInfo (conf :: Axi4WriteAddressConfig) (userType :: Type)
  = Axi4WriteAddressInfo
  { -- | Id
    _awiid :: !(BitVector (AWIdWidth conf))

    -- | Address
  , _awiaddr :: !(BitVector (AWAddrWidth conf))

    -- | Region
  , _awiregion :: !(RegionType (AWKeepRegion conf))

    -- | Burst size
  , _awisize :: !(SizeType (AWKeepSize conf))

    -- | Lock type
  , _awilock :: !(LockType (AWKeepLock conf))

    -- | Cache type
  , _awicache :: !(CacheType (AWKeepCache conf))

    -- | Protection type
  , _awiprot :: !(PermissionsType (AWKeepPermissions conf))

    -- | QoS value
  , _awiqos :: !(QosType (AWKeepQos conf))

    -- | User data
  , _awiuser :: !userType
  }
  deriving (Generic)

deriving instance
  ( GoodAxi4WriteAddressConfig conf
  , Show userType ) =>
  Show (Axi4WriteAddressInfo conf userType)

deriving instance
  ( GoodAxi4WriteAddressConfig conf
  , NFDataX userType ) =>
  NFDataX (Axi4WriteAddressInfo conf userType)

-- | Convert 'M2S_WriteAddress' to 'Axi4WriteAddressInfo', dropping some info
axi4WriteAddrMsgToWriteAddrInfo :: M2S_WriteAddress conf userType -> Axi4WriteAddressInfo conf userType
axi4WriteAddrMsgToWriteAddrInfo M2S_NoWriteAddress = errorX "Expected WriteAddress"
axi4WriteAddrMsgToWriteAddrInfo M2S_WriteAddress{..}
  = Axi4WriteAddressInfo
  { _awiid     = _awid
  , _awiaddr   = _awaddr
  , _awiregion = _awregion
  , _awisize   = _awsize
  , _awilock   = _awlock
  , _awicache  = _awcache
  , _awiprot   = _awprot
  , _awiqos    = _awqos
  , _awiuser   = _awuser
  }

-- | Convert 'Axi4WriteAddressInfo' to 'M2S_WriteAddress', adding some info
axi4WriteAddrMsgFromWriteAddrInfo
  :: BurstLengthType (AWKeepBurstLength conf)
  -> BurstType (AWKeepBurst conf)
  -> Axi4WriteAddressInfo conf userType
  -> M2S_WriteAddress conf userType
axi4WriteAddrMsgFromWriteAddrInfo _awlen _awburst Axi4WriteAddressInfo{..}
  = M2S_WriteAddress
  { _awid     = _awiid
  , _awaddr   = _awiaddr
  , _awregion = _awiregion
  , _awsize   = _awisize
  , _awlock   = _awilock
  , _awcache  = _awicache
  , _awprot   = _awiprot
  , _awqos    = _awiqos
  , _awuser   = _awiuser
  , _awlen, _awburst
  }


-- | Data carried along 'Axi4ReadAddress' channel which is put in control of the user,
-- rather than being managed by the 'DfLike' instances.
-- Matches up one-to-one with the fields of 'M2S_ReadAddress' except for
-- '_arlen', '_arsize', and '_arburst'.
data Axi4ReadAddressInfo (conf :: Axi4ReadAddressConfig) (userType :: Type)
  = Axi4ReadAddressInfo
  { -- | Id
    _ariid :: !(BitVector (ARIdWidth conf))

    -- | Address
  , _ariaddr :: !(BitVector (ARAddrWidth conf))

    -- | Region
  , _ariregion :: !(RegionType (ARKeepRegion conf))

    -- | Burst length
  , _arilen :: !(BurstLengthType (ARKeepBurstLength conf))

    -- | Burst size
  , _arisize :: !(SizeType (ARKeepSize conf))

    -- | Burst type
  , _ariburst :: !(BurstType (ARKeepBurst conf))

    -- | Lock type
  , _arilock :: !(LockType (ARKeepLock conf))

    -- | Cache type
  , _aricache :: !(CacheType (ARKeepCache conf))

    -- | Protection type
  , _ariprot :: !(PermissionsType (ARKeepPermissions conf))

    -- | QoS value
  , _ariqos :: !(QosType (ARKeepQos conf))

    -- | User data
  , _ariuser :: !userType
  }
  deriving (Generic)

deriving instance
  ( GoodAxi4ReadAddressConfig conf
  , Show userType ) =>
  Show (Axi4ReadAddressInfo conf userType)

deriving instance
  ( GoodAxi4ReadAddressConfig conf
  , NFDataX userType ) =>
  NFDataX (Axi4ReadAddressInfo conf userType)

-- | Convert 'M2S_ReadAddress' to 'Axi4ReadAddressInfo', dropping some info
axi4ReadAddrMsgToReadAddrInfo :: M2S_ReadAddress conf userType -> Axi4ReadAddressInfo conf userType
axi4ReadAddrMsgToReadAddrInfo M2S_NoReadAddress = errorX "Expected ReadAddress"
axi4ReadAddrMsgToReadAddrInfo M2S_ReadAddress{..}
  = Axi4ReadAddressInfo
  { _ariid     = _arid
  , _ariaddr   = _araddr
  , _ariregion = _arregion
  , _arilen    = _arlen
  , _arisize   = _arsize
  , _ariburst  = _arburst
  , _arilock   = _arlock
  , _aricache  = _arcache
  , _ariprot   = _arprot
  , _ariqos    = _arqos
  , _ariuser   = _aruser
  }

-- | Convert 'Axi4ReadAddressInfo' to 'M2S_ReadAddress', adding some info
axi4ReadAddrMsgFromReadAddrInfo
  :: Axi4ReadAddressInfo conf userType -> M2S_ReadAddress conf userType
axi4ReadAddrMsgFromReadAddrInfo Axi4ReadAddressInfo{..}
  = M2S_ReadAddress
  { _arid     = _ariid
  , _araddr   = _ariaddr
  , _arregion = _ariregion
  , _arlen    = _arilen
  , _arsize   = _arisize
  , _arburst  = _ariburst
  , _arlock   = _arilock
  , _arcache  = _aricache
  , _arprot   = _ariprot
  , _arqos    = _ariqos
  , _aruser   = _ariuser
  }


-- Fifo classes for Axi4 subordinate port

-- Does not support burst modes other than fixed.
instance
  ( GoodAxi4WriteAddressConfig confAW
  , GoodAxi4WriteDataConfig confW
  , GoodAxi4WriteResponseConfig confB
  , NFDataX userAW
  , NFDataX userB
  , AWIdWidth confAW ~ BIdWidth confB ) =>
  DfLike
    (Reverse (Axi4WriteAddress dom confAW userAW),
     Reverse (Axi4WriteData dom confW userW),
     Axi4WriteResponse dom confB userB)
    where

  type Dom
    (Reverse (Axi4WriteAddress dom confAW userAW),
     Reverse (Axi4WriteData dom confW userW),
     Axi4WriteResponse dom confB userB)
    = dom
  type BwdPayload
    (Reverse (Axi4WriteAddress dom confAW userAW),
     Reverse (Axi4WriteData dom confW userW),
     Axi4WriteResponse dom confB userB)
    = ( Axi4WriteAddressInfo confAW userAW
      , BurstLengthType (AWKeepBurstLength confAW)
      , BurstType (AWKeepBurst confAW)
      , StrictStrobeType (WNBytes confW) (WKeepStrobe confW)
      , userW )
  type FwdPayload
    (Reverse (Axi4WriteAddress dom confAW userAW),
     Reverse (Axi4WriteData dom confW userW),
     Axi4WriteResponse dom confB userB)
    = (ResponseType (BKeepResponse confB), userB)

  toDfCircuit _ = toDfCircuitHelper s0 blankOtp stateFn where
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
            (Just _bid, Just (_bresp, _buser)) -> (S2M_WriteResponse{..}, _bready wRespAck)
            _ -> (S2M_NoWriteResponse, False)
      when dfAckOut $ put (a, Nothing)
      P.pure (wRespVal, dfAckOut)

instance
  ( GoodAxi4WriteAddressConfig confAW
  , GoodAxi4WriteDataConfig confW
  , GoodAxi4WriteResponseConfig confB
  , NFDataX userAW
  , NFDataX userB
  , AWIdWidth confAW ~ BIdWidth confB ) =>
  ImplicitInfoClass
    (Reverse (Axi4WriteAddress dom confAW userAW),
     Reverse (Axi4WriteData dom confW userW),
     Axi4WriteResponse dom confB userB)
    where

  type ImplicitInfoBwdPayload
    (Reverse (Axi4WriteAddress dom confAW userAW),
     Reverse (Axi4WriteData dom confW userW),
     Axi4WriteResponse dom confB userB)
    = StrictStrobeType (WNBytes confW) (WKeepStrobe confW)

  type ImplicitInfoFwdParam
    (Reverse (Axi4WriteAddress dom confAW userAW),
     Reverse (Axi4WriteData dom confW userW),
     Axi4WriteResponse dom confB userB)
    = (ResponseType (BKeepResponse confB), userB)

  mapBwd _ (_, _, _, a, _) = a
  mapFwd (_, a) () = a

instance
  ( GoodAxi4ReadAddressConfig confAR
  , GoodAxi4ReadDataConfig confR
  , NFDataX userR
  , NFDataX dat
  , ARIdWidth confAR ~ RIdWidth confR ) =>
  DfLike
    (Reverse (Axi4ReadAddress dom confAR userAR),
     Axi4ReadData dom confR userR dat)
    where

  type Dom
    (Reverse (Axi4ReadAddress dom confAR userAR),
     Axi4ReadData dom confR userR dat)
     = dom
  type BwdPayload -- sent at the beginning of each burst (NOT each transfer)
    (Reverse (Axi4ReadAddress dom confAR userAR),
     Axi4ReadData dom confR userR dat)
     = Axi4ReadAddressInfo confAR userAR
  type FwdPayload
    (Reverse (Axi4ReadAddress dom confAR userAR),
     Axi4ReadData dom confR userR dat)
     = (dat, userR, ResponseType (RKeepResponse confR))

  toDfCircuit _ = toDfCircuitHelper s0 blankOtp stateFn where
    -- ( burst len left in transfer
    -- , read id currently replying to )
    s0 = (0, errorX "DfLike for Axi4: No initial value for read id" )

    blankOtp = (S2M_ReadAddress { _arready = False }, S2M_NoReadData)

    stateFn (addrVal, dataAck) dfAckIn dfDatIn = do
      (addrAck, dfDatOut) <- processAddr addrVal dfAckIn
      (dataVal, dfAckOut) <- sendData dfDatIn dataAck
      P.pure ((addrAck,dataVal),dfDatOut,dfAckOut)

    processAddr M2S_NoReadAddress _ = P.pure (S2M_ReadAddress { _arready = False }, Nothing)
    processAddr msg dfAckIn = do
      (burstLenLeft,_) <- get
      when (burstLenLeft == 0) $ put (succResizing $ fromMaybe 0 (fromKeepType $ _arlen msg), _arid msg)
      P.pure (S2M_ReadAddress{ _arready = burstLenLeft == 0 && dfAckIn }, Just (axi4ReadAddrMsgToReadAddrInfo msg))

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

instance
  ( GoodAxi4ReadAddressConfig confAR
  , GoodAxi4ReadDataConfig confR
  , NFDataX userR
  , NFDataX dat
  , ARIdWidth confAR ~ RIdWidth confR ) =>
  ImplicitInfoClass
    (Reverse (Axi4ReadAddress dom confAR userAR),
     Axi4ReadData dom confR userR dat)
    where

  type ImplicitInfoFwdPayload
    (Reverse (Axi4ReadAddress dom confAR userAR),
     Axi4ReadData dom confR userR dat)
     = dat

  type ImplicitInfoFwdParam
    (Reverse (Axi4ReadAddress dom confAR userAR),
     Axi4ReadData dom confR userR dat)
     = (userR, ResponseType (RKeepResponse confR))

  mapFwd (_, (b, c)) a = (a, b, c)
  mapBwd _ _ = ()


-- Fifo classes for Axi4 manager port

-- Only allows for burst length of 1
instance
  ( GoodAxi4WriteAddressConfig confAW
  , GoodAxi4WriteDataConfig confW
  , GoodAxi4WriteResponseConfig confB ) =>
  DfLike
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
    = (Axi4WriteAddressInfo confAW userAW, StrictStrobeType (WNBytes confW) (WKeepStrobe confW), userW)

  type BwdPayload
    (Axi4WriteAddress dom confAW userAW,
     Axi4WriteData dom confW userW,
     Reverse (Axi4WriteResponse dom confB userB))
    = (ResponseType (BKeepResponse confB), userB)

  toDfCircuit (_, param) = toDfCircuitHelper s0 blankOtp (stateFn param) where
    s0 = (False, False) -- address received, data received

    blankOtp =
      ( M2S_NoWriteAddress
      , M2S_NoWriteData
      , M2S_WriteResponse { _bready = False }
      )

    stateFn _ (addrAck, dataAck, respVal) dfAckIn dfDatIn = do
      addrMsg <- sendAddr addrAck dfDatIn
      (dataMsg, dfAckOut) <- sendData dataAck dfDatIn
      (respAck, dfDatOut) <- receiveResp respVal dfAckIn
      P.pure ((addrMsg, dataMsg, respAck), dfDatOut, dfAckOut)

    sendAddr _ Nothing = P.pure M2S_NoWriteAddress
    sendAddr S2M_WriteAddress{_awready} (Just (info, _, _)) = do
      (addrReceived, b) <- get
      put (_awready || addrReceived, b)
      P.pure $ if addrReceived then M2S_NoWriteAddress
               else axi4WriteAddrMsgFromWriteAddrInfo (toKeepType 1) (toKeepType BmFixed) info

    sendData _ Nothing = P.pure (M2S_NoWriteData, False)
    sendData S2M_WriteData{_wready} (Just (_, dat, user)) = do
      (addrReceived, dataReceived) <- get
      put (addrReceived, _wready || dataReceived)
      P.pure $ if (not addrReceived || dataReceived) then (M2S_NoWriteData, False) else (M2S_WriteData
        { _wdata = dat
        , _wlast = True
        , _wuser = user
        }, _wready)

    receiveResp S2M_NoWriteResponse _ = P.pure (M2S_WriteResponse { _bready = False }, Nothing)
    receiveResp S2M_WriteResponse{_bresp,_buser} dfAckIn = do
      (_, dataReceived) <- get
      let shouldAckResponse = dataReceived && dfAckIn
      when shouldAckResponse $ put (False, False)
      P.pure (M2S_WriteResponse { _bready = shouldAckResponse }, Just (_bresp,_buser))

instance
  ( GoodAxi4WriteAddressConfig confAW
  , GoodAxi4WriteDataConfig confW
  , GoodAxi4WriteResponseConfig confB ) =>
  ImplicitInfoClass
    (Axi4WriteAddress dom confAW userAW,
     Axi4WriteData dom confW userW,
     Reverse (Axi4WriteResponse dom confB userB))
    where

  type ImplicitInfoFwdPayload
    (Axi4WriteAddress dom confAW userAW,
     Axi4WriteData dom confW userW,
     Reverse (Axi4WriteResponse dom confB userB))
    = StrictStrobeType (WNBytes confW) (WKeepStrobe confW)

  type ImplicitInfoFwdParam
    (Axi4WriteAddress dom confAW userAW,
     Axi4WriteData dom confW userW,
     Reverse (Axi4WriteResponse dom confB userB))
    = (Axi4WriteAddressInfo confAW userAW, userW)

  mapFwd (_, (a, c)) b = (a, b, c)
  mapBwd _ _ = ()

instance
  ( GoodAxi4ReadAddressConfig confAR
  , GoodAxi4ReadDataConfig confR ) =>
  DfLike
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
     = (dat, userR)

  type FwdPayload
    (Axi4ReadAddress dom confAR dataAR,
     Reverse (Axi4ReadData dom confR userR dat))
     = Axi4ReadAddressInfo confAR dataAR

  toDfCircuit (_, param) = toDfCircuitHelper s0 blankOtp (stateFn param) where
    s0 = ()

    blankOtp =
      ( M2S_NoReadAddress
      , M2S_ReadData { _rready = False }
      )

    stateFn _ (addrAck, readVal) dfAckIn dfDatIn =
      let readAddrMsg = processAddrInfo dfDatIn
      in  P.pure ((readAddrMsg, M2S_ReadData { _rready = dfAckIn }), processReadVal readVal, getDfAckOut addrAck readAddrMsg)

    processAddrInfo = maybe M2S_NoReadAddress axi4ReadAddrMsgFromReadAddrInfo

    processReadVal S2M_NoReadData = Nothing
    processReadVal S2M_ReadData{..} = Just (_rdata, _ruser)

    getDfAckOut _ M2S_NoReadAddress = False
    getDfAckOut addrAck _ = _arready addrAck

instance
  ( GoodAxi4ReadAddressConfig confAR
  , GoodAxi4ReadDataConfig confR ) =>
  ImplicitInfoClass
    (Axi4ReadAddress dom confAR dataAR,
     Reverse (Axi4ReadData dom confR userR dat))
    where

  type ImplicitInfoBwdPayload
    (Axi4ReadAddress dom confAR dataAR,
     Reverse (Axi4ReadData dom confR userR dat))
     = dat

  type ImplicitInfoFwdParam
    (Axi4ReadAddress dom confAR dataAR,
     Reverse (Axi4ReadData dom confR userR dat))
     = Axi4ReadAddressInfo confAR dataAR

  mapFwd (_, a) () = a
  mapBwd _ (a, _) = a


-- | Convert 'DfLike' into a /one-way/ 'Df' port,
-- at the data input end
dfToDfLikeInp
  :: DfLike df
  => HiddenClockResetEnable (Dom df)
  => (Proxy df, DfLikeParam df)
  -> Circuit (Reverse df) (Df (Dom df) (BwdPayload df))
dfToDfLikeInp = mapCircuit id id P.fst (, P.pure NoData) . fromDfCircuit

-- | Convert 'DfLike' into a /one-way/ 'Df' port,
-- at the data output end
dfToDfLikeOtp
  :: DfLike df
  => HiddenClockResetEnable (Dom df)
  => (Proxy df, DfLikeParam df)
  -> Circuit (Df (Dom df) (FwdPayload df)) df
dfToDfLikeOtp = mapCircuit (, P.pure (Ack False)) P.fst id id . toDfCircuit

-- 'toDfCircuit', but the 'DfLike' is inside a 'Vec'
vecToDfLike
  :: DfLike df
  => HiddenClockResetEnable (Dom df)
  => KnownNat n
  => Vec n (Proxy df, DfLikeParam df)
  -> Circuit (Vec n (Df (Dom df) (FwdPayload df)), Vec n (Reverse (Df (Dom df) (BwdPayload df)))) (Vec n df)
vecToDfLike params = mapCircuit (uncurry C.zip) unzip id id $ vecCircuits $ toDfCircuit <$> params

-- 'fromDfCircuit', but the 'DfLike' is inside a 'Vec'
vecFromDfLike
  :: DfLike df
  => HiddenClockResetEnable (Dom df)
  => KnownNat n
  => Vec n (Proxy df, DfLikeParam df)
  -> Circuit (Vec n (Reverse df)) (Vec n (Df (Dom df) (BwdPayload df)), Vec n (Reverse (Df (Dom df) (FwdPayload df))))
vecFromDfLike params = mapCircuit id id unzip (uncurry C.zip) $ vecCircuits $ fromDfCircuit <$> params

-- 'toDfCircuit', but on a pair of 'DfLike's
tupToDfLike
  :: DfLike dfA
  => DfLike dfB
  => Dom dfA ~ Dom dfB
  => HiddenClockResetEnable (Dom dfA)
  => ((Proxy dfA, DfLikeParam dfA), (Proxy dfB, DfLikeParam dfB))
  -> Circuit ((Df (Dom dfA) (FwdPayload dfA), Df (Dom dfB) (FwdPayload dfB)), (Reverse (Df (Dom dfA) (BwdPayload dfA)), Reverse (Df (Dom dfB) (BwdPayload dfB))))
             (dfA, dfB)
tupToDfLike (argsA, argsB) = mapCircuit f f id id $ tupCircuits (toDfCircuit argsA) (toDfCircuit argsB) where
  f ((a,b),(c,d)) = ((a,c),(b,d))

-- 'fromDfCircuit', but on a pair of 'DfLike's
tupFromDfLike
  :: DfLike dfA
  => DfLike dfB
  => Dom dfA ~ Dom dfB
  => HiddenClockResetEnable (Dom dfA)
  => ((Proxy dfA, DfLikeParam dfA), (Proxy dfB, DfLikeParam dfB))
  -> Circuit (Reverse (dfA, dfB))
             ((Df (Dom dfA) (BwdPayload dfA), Df (Dom dfB) (BwdPayload dfB)), (Reverse (Df (Dom dfA) (FwdPayload dfA)), Reverse (Df (Dom dfB) (FwdPayload dfB))))
tupFromDfLike (argsA, argsB) = mapCircuit id id f f $ tupCircuits (fromDfCircuit argsA) (fromDfCircuit argsB) where
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

-- | Preserves 'Df' data unmodified.
-- Potentially useful for converting between protocols.
convert ::
  ( DfLike dfA
  , DfLike dfB
  , BwdPayload dfA ~ FwdPayload dfB
  , FwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA) ) =>
  (Proxy dfA, DfLikeParam dfA) ->
  (Proxy dfB, DfLikeParam dfB) ->
  Circuit (Reverse dfA) dfB
convert dfA dfB
  =  fromDfCircuit dfA
  |> toDfCircuit dfB

-- | Like 'P.map'
map ::
  ( DfLike dfA
  , DfLike dfB
  , FwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA) ) =>
  (Proxy dfA, DfLikeParam dfA) ->
  (Proxy dfB, DfLikeParam dfB) ->
  (BwdPayload dfA -> FwdPayload dfB) ->
  Circuit (Reverse dfA) dfB
map dfA dfB f
  =  fromDfCircuit dfA
  |> tupCircuits (Df.map f) idC
  |> toDfCircuit dfB

-- | Like 'P.fst'
fst ::
  ( DfLike dfA
  , DfLike dfB
  , FwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , BwdPayload dfA ~ (a, b)
  , FwdPayload dfB ~ a ) =>
  (Proxy dfA, DfLikeParam dfA) ->
  (Proxy dfB, DfLikeParam dfB) ->
  Circuit (Reverse dfA) dfB
fst dfA dfB
  =  fromDfCircuit dfA
  |> tupCircuits Df.fst idC
  |> toDfCircuit dfB

-- | Like 'P.fst'
snd ::
  ( DfLike dfA
  , DfLike dfB
  , FwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , BwdPayload dfA ~ (a, b)
  , FwdPayload dfB ~ b ) =>
  (Proxy dfA, DfLikeParam dfA) ->
  (Proxy dfB, DfLikeParam dfB) ->
  Circuit (Reverse dfA) dfB
snd dfA dfB
  =  fromDfCircuit dfA
  |> tupCircuits Df.snd idC
  |> toDfCircuit dfB

-- | Like 'Data.Bifunctor.bimap'
bimap ::
  ( DfLike dfA
  , DfLike dfB
  , FwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , B.Bifunctor p
  , BwdPayload dfA ~ p a c
  , FwdPayload dfB ~ p b d ) =>
  (Proxy dfA, DfLikeParam dfA) ->
  (Proxy dfB, DfLikeParam dfB) ->
  (a -> b) ->
  (c -> d) ->
  Circuit (Reverse dfA) dfB
bimap dfA dfB f g
  =  fromDfCircuit dfA
  |> tupCircuits (Df.bimap f g) idC
  |> toDfCircuit dfB

-- | Like 'Data.Bifunctor.first'
first ::
  ( DfLike dfA
  , DfLike dfB
  , FwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , B.Bifunctor p
  , BwdPayload dfA ~ p a c
  , FwdPayload dfB ~ p b c ) =>
  (Proxy dfA, DfLikeParam dfA) ->
  (Proxy dfB, DfLikeParam dfB) ->
  (a -> b) ->
  Circuit (Reverse dfA) dfB
first dfA dfB f
  =  fromDfCircuit dfA
  |> tupCircuits (Df.first f) idC
  |> toDfCircuit dfB

-- | Like 'Data.Bifunctor.second'
second ::
  ( DfLike dfA
  , DfLike dfB
  , FwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , B.Bifunctor p
  , BwdPayload dfA ~ p a b
  , FwdPayload dfB ~ p a c ) =>
  (Proxy dfA, DfLikeParam dfA) ->
  (Proxy dfB, DfLikeParam dfB) ->
  (b -> c) ->
  Circuit (Reverse dfA) dfB
second dfA dfB f
  =  fromDfCircuit dfA
  |> tupCircuits (Df.second f) idC
  |> toDfCircuit dfB

-- | Acknowledge but ignore data from LHS protocol. Send a static value /b/.
const ::
  ( DfLike dfA
  , DfLike dfB
  , FwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA) ) =>
  (Proxy dfA, DfLikeParam dfA) ->
  (Proxy dfB, DfLikeParam dfB) ->
  FwdPayload dfB ->
  Circuit (Reverse dfA) dfB
const dfA dfB b
  =  fromDfCircuit dfA
  |> tupCircuits (Df.const b) idC
  |> toDfCircuit dfB

-- | Drive a constant value composed of /a/.
pure ::
  ( DfLike df
  , HiddenClockResetEnable (Dom df) ) =>
  (Proxy df, DfLikeParam df) ->
  FwdPayload df ->
  Circuit () df
pure df a
  =  Df.pure a
  |> dfToDfLikeOtp df

-- | Ignore incoming data
void ::
  ( DfLike df
  , HiddenClockResetEnable (Dom df) ) =>
  (Proxy df, DfLikeParam df) ->
  Circuit (Reverse df) ()
void df = dfToDfLikeInp df
  |> Df.void

-- | Like 'Data.Maybe.catMaybes'
catMaybes ::
  ( DfLike dfA
  , DfLike dfB
  , FwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , BwdPayload dfA ~ Maybe (FwdPayload dfB) ) =>
  (Proxy dfA, DfLikeParam dfA) ->
  (Proxy dfB, DfLikeParam dfB) ->
  Circuit (Reverse dfA) dfB
catMaybes dfA dfB
  =  fromDfCircuit dfA
  |> tupCircuits (Df.catMaybes) idC
  |> toDfCircuit dfB

-- | Like 'Data.Maybe.mapMaybe'
mapMaybe ::
  ( DfLike dfA
  , DfLike dfB
  , FwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , NFDataX (FwdPayload dfB) ) =>
  (Proxy dfA, DfLikeParam dfA) ->
  (Proxy dfB, DfLikeParam dfB) ->
  (BwdPayload dfA -> Maybe (FwdPayload dfB)) ->
  Circuit (Reverse dfA) dfB
mapMaybe dfA dfB f
  =  fromDfCircuit dfA
  |> tupCircuits (Df.mapMaybe f) idC
  |> toDfCircuit dfB

-- | Like 'P.filter'
filter ::
  ( DfLike dfA
  , DfLike dfB
  , FwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , BwdPayload dfA ~ FwdPayload dfB ) =>
  (Proxy dfA, DfLikeParam dfA) ->
  (Proxy dfB, DfLikeParam dfB) ->
  (BwdPayload dfA -> Bool) ->
  Circuit (Reverse dfA) dfB
filter dfA dfB f
  =  fromDfCircuit dfA
  |> tupCircuits (Df.filter f) idC
  |> toDfCircuit dfB

-- | Like 'Data.Either.Combinators.mapLeft'
mapLeft ::
  ( DfLike dfA
  , DfLike dfB
  , FwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , BwdPayload dfA ~ Either a c
  , FwdPayload dfB ~ Either b c ) =>
  (Proxy dfA, DfLikeParam dfA) ->
  (Proxy dfB, DfLikeParam dfB) ->
  (a -> b) ->
  Circuit (Reverse dfA) dfB
mapLeft dfA dfB f
  =  fromDfCircuit dfA
  |> tupCircuits (Df.mapLeft f) idC
  |> toDfCircuit dfB

-- | Like 'Data.Either.Combinators.mapRight'
mapRight ::
  ( DfLike dfA
  , DfLike dfB
  , FwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , BwdPayload dfA ~ Either a b
  , FwdPayload dfB ~ Either a c ) =>
  (Proxy dfA, DfLikeParam dfA) ->
  (Proxy dfB, DfLikeParam dfB) ->
  (b -> c) ->
  Circuit (Reverse dfA) dfB
mapRight dfA dfB f
  =  fromDfCircuit dfA
  |> tupCircuits (Df.mapRight f) idC
  |> toDfCircuit dfB

-- | Like 'Data.Either.either'
either ::
  ( DfLike dfA
  , DfLike dfB
  , FwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , BwdPayload dfA ~ Either a b ) =>
  (Proxy dfA, DfLikeParam dfA) ->
  (Proxy dfB, DfLikeParam dfB) ->
  (a -> FwdPayload dfB) ->
  (b -> FwdPayload dfB) ->
  Circuit (Reverse dfA) dfB
either dfA dfB f g
  =  fromDfCircuit dfA
  |> tupCircuits (Df.either f g) idC
  |> toDfCircuit dfB

-- | Like 'P.zipWith'. Any data not in /Payload/ is copied from stream A.
zipWith ::
  ( DfLike dfA
  , DfLike dfB
  , DfLike dfC
  , FwdPayload dfA ~ BwdPayload dfC
  , FwdPayload dfB ~ BwdPayload dfC
  , Dom dfA ~ Dom dfB
  , Dom dfA ~ Dom dfC
  , HiddenClockResetEnable (Dom dfA) ) =>
  ((Proxy dfA, DfLikeParam dfA), (Proxy dfB, DfLikeParam dfB)) ->
  (Proxy dfC, DfLikeParam dfC) ->
  (BwdPayload dfA -> BwdPayload dfB -> FwdPayload dfC) ->
  Circuit (Reverse (dfA, dfB)) dfC
zipWith dfAB dfC f
  =  tupFromDfLike dfAB
  |> coerceCircuit
  (  tupCircuits
     (Df.zipWith f)
     (reverseCircuit $ Df.roundrobin |> vecToTup))
  |> toDfCircuit dfC

-- | Like 'P.zip'
zip ::
  ( DfLike dfA
  , DfLike dfB
  , DfLike dfC
  , FwdPayload dfA ~ BwdPayload dfC
  , FwdPayload dfB ~ BwdPayload dfC
  , Dom dfA ~ Dom dfB
  , Dom dfA ~ Dom dfC
  , HiddenClockResetEnable (Dom dfA)
  , FwdPayload dfC ~ (BwdPayload dfA, BwdPayload dfB) ) =>
  ((Proxy dfA, DfLikeParam dfA), (Proxy dfB, DfLikeParam dfB)) ->
  (Proxy dfC, DfLikeParam dfC) ->
  Circuit (Reverse (dfA, dfB)) dfC
zip dfAB dfC
  =  tupFromDfLike dfAB
  |> coerceCircuit
  (  tupCircuits
     Df.zip
     (reverseCircuit $ Df.roundrobin |> vecToTup))
  |> toDfCircuit dfC

-- | Like 'P.partition'
partition ::
  ( DfLike dfA
  , DfLike dfB
  , DfLike dfC
  , FwdPayload dfA ~ BwdPayload dfB
  , FwdPayload dfA ~ BwdPayload dfC
  , Dom dfA ~ Dom dfB
  , Dom dfA ~ Dom dfC
  , HiddenClockResetEnable (Dom dfA)
  , BwdPayload dfA ~ FwdPayload dfB
  , BwdPayload dfA ~ FwdPayload dfC ) =>
  (Proxy dfA, DfLikeParam dfA) ->
  ((Proxy dfB, DfLikeParam dfB), (Proxy dfC, DfLikeParam dfC)) ->
  (BwdPayload dfA -> Bool) ->
  Circuit (Reverse dfA) (dfB, dfC)
partition dfA dfBC f
  =  fromDfCircuit dfA
  |> coerceCircuit
  (  tupCircuits
     (Df.partition f)
     (reverseCircuit $ tupToVec |> Df.roundrobinCollect Df.Parallel))
  |> tupToDfLike dfBC

-- | Route a DfLike stream to another corresponding to the index
route ::
  ( DfLike dfA
  , DfLike dfB
  , FwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , BwdPayload dfA ~ (Index n, FwdPayload dfB)
  , KnownNat n
  , 1 <= n ) =>
  (Proxy dfA, DfLikeParam dfA) ->
  Vec n (Proxy dfB, DfLikeParam dfB) ->
  Circuit (Reverse dfA) (Vec n dfB)
route dfA dfB
  =  fromDfCircuit dfA
  |> coerceCircuit
  (  tupCircuits
     Df.route
     (reverseCircuit (Df.roundrobinCollect Df.Parallel)))
  |> vecToDfLike dfB

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

-- | Select data from the channel indicated by the DfLike stream carrying
-- @Index n@.
select ::
  ( DfLike dfA
  , DfLike dfB
  , DfLike dfC
  , FwdPayload dfA ~ BwdPayload dfC
  , FwdPayload dfB ~ BwdPayload dfC
  , Dom dfA ~ Dom dfB
  , Dom dfA ~ Dom dfC
  , HiddenClockResetEnable (Dom dfA)
  , BwdPayload dfA ~ FwdPayload dfC
  , BwdPayload dfB ~ Index n
  , KnownNat n ) =>
  (Vec n (Proxy dfA, DfLikeParam dfA), (Proxy dfB, DfLikeParam dfB)) ->
  (Proxy dfC, DfLikeParam dfC) ->
  Circuit (Vec n (Reverse dfA), Reverse dfB) dfC
select (dfA, dfB) dfC
  =  tupCircuits (vecFromDfLike dfA) (fromDfCircuit dfB)
  |> selectHelperA
  |> coerceCircuit
  (  tupCircuits
     Df.select
     (reverseCircuit $ Df.roundrobin |> selectHelperB))
  |> toDfCircuit dfC

-- | Select /selectN/ samples from channel /n/.
selectN ::
  ( DfLike dfA
  , DfLike dfB
  , DfLike dfC
  , FwdPayload dfA ~ BwdPayload dfC
  , FwdPayload dfB ~ BwdPayload dfC
  , Dom dfA ~ Dom dfB
  , Dom dfA ~ Dom dfC
  , HiddenClockResetEnable (Dom dfA)
  , BwdPayload dfA ~ FwdPayload dfC
  , BwdPayload dfB ~ (Index n, Index selectN)
  , KnownNat n
  , KnownNat selectN ) =>
  (Vec n (Proxy dfA, DfLikeParam dfA), (Proxy dfB, DfLikeParam dfB)) ->
  (Proxy dfC, DfLikeParam dfC) ->
  Circuit (Vec n (Reverse dfA), Reverse dfB) dfC
selectN (dfA, dfB) dfC
  =  tupCircuits (vecFromDfLike dfA) (fromDfCircuit dfB)
  |> selectHelperA
  |> coerceCircuit
  (  tupCircuits
     Df.selectN
     (reverseCircuit $ Df.roundrobin |> selectHelperB))
  |> toDfCircuit dfC

-- | Selects samples from channel /n/ until the predicate holds. The cycle in
-- which the predicate turns true is included.
selectUntil ::
  ( DfLike dfA
  , DfLike dfB
  , DfLike dfC
  , FwdPayload dfA ~ BwdPayload dfC
  , FwdPayload dfB ~ BwdPayload dfC
  , Dom dfA ~ Dom dfB
  , Dom dfA ~ Dom dfC
  , HiddenClockResetEnable (Dom dfA)
  , BwdPayload dfA ~ FwdPayload dfC
  , BwdPayload dfB ~ Index n
  , KnownNat n ) =>
  (Vec n (Proxy dfA, DfLikeParam dfA), (Proxy dfB, DfLikeParam dfB)) ->
  (Proxy dfC, DfLikeParam dfC) ->
  (BwdPayload dfA -> Bool) ->
  Circuit (Vec n (Reverse dfA), Reverse dfB) dfC
selectUntil (dfA, dfB) dfC f
  =  tupCircuits (vecFromDfLike dfA) (fromDfCircuit dfB)
  |> selectHelperA
  |> coerceCircuit
  (  tupCircuits
     (Df.selectUntil f)
     (reverseCircuit $ Df.roundrobin |> selectHelperB))
  |> toDfCircuit dfC

-- | Copy data of a single DfLike stream to multiple. LHS will only receive
-- an acknowledgement when all RHS receivers have acknowledged data.
fanout ::
  ( DfLike dfA
  , DfLike dfB
  , FwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , BwdPayload dfA ~ FwdPayload dfB
  , NFDataX (BwdPayload dfA)
  , KnownNat numB
  , numB ~ (decNumB + 1) ) =>
  (Proxy dfA, DfLikeParam dfA) ->
  Vec numB (Proxy dfB, DfLikeParam dfB) ->
  Circuit (Reverse dfA) (Vec numB dfB)
fanout dfA dfB
  =  fromDfCircuit dfA
  |> coerceCircuit
  (  tupCircuits
     Df.fanout
     (reverseCircuit (Df.roundrobinCollect Df.Parallel)))
  |> vecToDfLike dfB

-- | Merge data of multiple streams using a user supplied function
fanin ::
  ( DfLike dfA
  , DfLike dfB
  , FwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , BwdPayload dfA ~ FwdPayload dfB
  , NFDataX (BwdPayload dfA)
  , KnownNat numA
  , numA ~ (decNumA + 1) ) =>
  Vec numA (Proxy dfA, DfLikeParam dfA) ->
  (Proxy dfB, DfLikeParam dfB) ->
  (BwdPayload dfA -> BwdPayload dfA -> BwdPayload dfA) ->
  Circuit (Vec numA (Reverse dfA)) dfB
fanin dfA dfB f
  =  vecFromDfLike dfA
  |> coerceCircuit
  (  tupCircuits
     (Df.fanin f)
     (reverseCircuit Df.roundrobin))
  |> toDfCircuit dfB

-- | Merge data of multiple streams using Monoid's '<>'.
mfanin ::
  ( DfLike dfA
  , DfLike dfB
  , FwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , BwdPayload dfA ~ FwdPayload dfB
  , NFDataX (BwdPayload dfA)
  , Monoid (BwdPayload dfA)
  , KnownNat numA
  , numA ~ (decNumA + 1) ) =>
  Vec numA (Proxy dfA, DfLikeParam dfA) ->
  (Proxy dfB, DfLikeParam dfB) ->
  Circuit (Vec numA (Reverse dfA)) dfB
mfanin dfA dfB
  =  vecFromDfLike dfA
  |> coerceCircuit
  (  tupCircuits
     Df.mfanin
     (reverseCircuit Df.roundrobin))
  |> toDfCircuit dfB

-- | Bundle a vector of DfLike streams into one.
bundleVec ::
  ( DfLike dfA
  , DfLike dfB
  , FwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , Vec n (BwdPayload dfA) ~ FwdPayload dfB
  , KnownNat n
  , n ~ (decN + 1) ) =>
  Vec n (Proxy dfA, DfLikeParam dfA) ->
  (Proxy dfB, DfLikeParam dfB) ->
  Circuit (Vec n (Reverse dfA)) dfB
bundleVec dfA dfB
  =  vecFromDfLike dfA
  |> coerceCircuit
  (  tupCircuits
     Df.bundleVec
     (reverseCircuit Df.roundrobin))
  |> toDfCircuit dfB

-- | Split up a DfLike stream of a vector into multiple independent DfLike streams.
unbundleVec ::
  ( DfLike dfA
  , DfLike dfB
  , FwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , BwdPayload dfA ~ Vec n (FwdPayload dfB)
  , NFDataX (FwdPayload dfB)
  , KnownNat n
  , 1 <= n ) =>
  (Proxy dfA, DfLikeParam dfA) ->
  Vec n (Proxy dfB, DfLikeParam dfB) ->
  Circuit (Reverse dfA) (Vec n dfB)
unbundleVec dfA dfB
  =  fromDfCircuit dfA
  |> coerceCircuit
  (  tupCircuits
     Df.unbundleVec
     (reverseCircuit (Df.roundrobinCollect Df.Parallel)))
  |> vecToDfLike dfB

-- | Distribute data across multiple components on the RHS. Useful if you want
-- to parallelize a workload across multiple (slow) workers. For optimal
-- throughput, you should make sure workers can accept data every /n/ cycles.
roundrobin ::
  ( DfLike dfA
  , DfLike dfB
  , FwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , BwdPayload dfA ~ FwdPayload dfB
  , KnownNat n
  , n ~ (decN + 1) ) =>
  (Proxy dfA, DfLikeParam dfA) ->
  Vec n (Proxy dfB, DfLikeParam dfB) ->
  Circuit (Reverse dfA) (Vec n dfB)
roundrobin dfA dfB
  =  fromDfCircuit dfA
  |> coerceCircuit
  (  tupCircuits
     Df.roundrobin
     (reverseCircuit (Df.roundrobinCollect Df.Parallel)))
  |> vecToDfLike dfB

-- | Opposite of 'roundrobin'. Useful to collect data from workers that only
-- produce a result with an interval of /n/ cycles.
roundrobinCollect ::
  ( DfLike dfA
  , DfLike dfB
  , FwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , BwdPayload dfA ~ FwdPayload dfB
  , KnownNat n
  , n ~ (decN + 1) ) =>
  Vec n (Proxy dfA, DfLikeParam dfA) ->
  (Proxy dfB, DfLikeParam dfB) ->
  Df.CollectMode ->
  Circuit (Vec n (Reverse dfA)) dfB
roundrobinCollect dfA dfB mode
  =  vecFromDfLike dfA
  |> coerceCircuit
  (  tupCircuits
     (Df.roundrobinCollect mode)
     (reverseCircuit Df.roundrobin))
  |> toDfCircuit dfB

-- | Place register on /forward/ part of a circuit.
registerFwd ::
  ( DfLike dfA
  , DfLike dfB
  , FwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , BwdPayload dfA ~ FwdPayload dfB
  , NFDataX (BwdPayload dfA) ) =>
  (Proxy dfA, DfLikeParam dfA) ->
  (Proxy dfB, DfLikeParam dfB) ->
  Circuit (Reverse dfA) dfB
registerFwd dfA dfB
  =  fromDfCircuit dfA
  |> tupCircuits (Df.registerFwd) idC
  |> toDfCircuit dfB

-- | Place register on /backward/ part of a circuit. This is implemented using a
-- in-logic two-element shift register.
registerBwd ::
  ( DfLike dfA
  , DfLike dfB
  , FwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , BwdPayload dfA ~ FwdPayload dfB
  , NFDataX (BwdPayload dfA) ) =>
  (Proxy dfA, DfLikeParam dfA) ->
  (Proxy dfB, DfLikeParam dfB) ->
  Circuit (Reverse dfA) dfB
registerBwd dfA dfB
  =  fromDfCircuit dfA
  |> tupCircuits (Df.registerBwd) idC
  |> toDfCircuit dfB

-- | A fifo buffer with user-provided depth.
-- Uses blockram to store data
fifo ::
  ( DfLike dfA
  , DfLike dfB
  , FwdPayload dfA ~ BwdPayload dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , KnownNat depth
  , BwdPayload dfA ~ FwdPayload dfB
  , NFDataX (BwdPayload dfA) ) =>
  (Proxy dfA, DfLikeParam dfA) ->
  (Proxy dfB, DfLikeParam dfB) ->
  SNat depth ->
  Circuit (Reverse dfA) dfB
fifo dfA dfB fifoDepth
  =  fromDfCircuit dfA
  |> tupCircuits (Df.fifo fifoDepth) idC
  |> toDfCircuit dfB where

-- | Emit values given in list. Emits no data while reset is asserted. Not
-- synthesizable.
drive ::
  ( DfLike dfA
  , HiddenClockResetEnable (Dom dfA) ) =>
  (Proxy dfA, DfLikeParam dfA) ->
  SimulationConfig ->
  [Maybe (FwdPayload dfA)] ->
  Circuit () dfA
drive dfA conf s0 = Df.drive conf s0 |> dfToDfLikeOtp dfA

-- | Sample protocol to a list of values. Drops values while reset is asserted.
-- Not synthesizable.
--
-- For a generalized version of 'sample', check out 'sampleC'.
sample ::
  ( DfLike dfB
  , HiddenClockResetEnable (Dom dfB) ) =>
  (Proxy dfB, DfLikeParam dfB) ->
  SimulationConfig ->
  Circuit () (Reverse dfB) ->
  [Maybe (BwdPayload dfB)]
sample dfB conf c = Df.sample conf (c |> dfToDfLikeInp dfB)

-- | Stall every valid Df packet with a given number of cycles. If there are
-- more valid packets than given numbers, passthrough all valid packets without
-- stalling. Not synthesizable.
--
-- For a generalized version of 'stall', check out 'stallC'.
stall ::
  ( DfLike dfA
  , DfLike dfB
  , Dom dfA ~ Dom dfB
  , HiddenClockResetEnable (Dom dfA)
  , BwdPayload dfA ~ FwdPayload dfB
  , HasCallStack ) =>
  (Proxy dfA, DfLikeParam dfA) ->
  (Proxy dfB, DfLikeParam dfB) ->
  SimulationConfig ->
  -- | Acknowledgement to send when LHS does not send data. Stall will act
  -- transparently when reset is asserted.
  StallAck ->
  -- Number of cycles to stall for every valid Df packet
  [Int] ->
  Circuit (Reverse dfA) dfB
stall dfA dfB conf stallAck stalls
  =  dfToDfLikeInp dfA
  |> Df.stall conf stallAck stalls
  |> dfToDfLikeOtp dfB

-- | Simulate a single domain protocol. Not synthesizable.
--
-- For a generalized version of 'simulate', check out 'Protocols.simulateC'.
--
-- You may notice that things seem to be "switched around"
-- in this function compared to others
-- (the @Circuit@ has @Reverse@ applied to its right side,
-- rather than its left, and we take the @FwdPayload@
-- of @dfA@ rather than @dfB@).
-- This is because we are taking a @Circuit@ as a parameter,
-- rather than returning a @Circuit@ like most other functions do.
simulate ::
  ( DfLike dfA
  , DfLike dfB
  , Dom dfA ~ Dom dfB
  , KnownDomain (Dom dfA)
  , HasCallStack ) =>
  (Proxy dfA, DfLikeParam dfA) ->
  (Proxy dfB, DfLikeParam dfB) ->
  -- | Simulation configuration. Use 'Data.Default.def' for sensible defaults.
  SimulationConfig ->
  -- | Circuit to simulate.
  ( Clock (Dom dfA) ->
    Reset (Dom dfA) ->
    Enable (Dom dfA) ->
    Circuit dfA (Reverse dfB) ) ->
  -- | Inputs
  [Maybe (FwdPayload dfA)] ->
  -- | Outputs
  [Maybe (BwdPayload dfB)]
simulate dfA dfB conf circ inputs = Df.simulate conf circ' inputs where
  circ' clk rst en
    =  withClockResetEnable clk rst en (dfToDfLikeOtp dfA)
    |> circ clk rst en
    |> withClockResetEnable clk rst en (dfToDfLikeInp dfB)
