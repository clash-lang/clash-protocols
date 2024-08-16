{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

-- | Circuits and utils for working with Standard mode wishbone circuits.
module Protocols.Wishbone.Standard where

import Clash.Prelude
import qualified Data.Bifunctor as B
import Protocols
import Protocols.Wishbone
import Prelude hiding (head, not, repeat, (!!), (&&), (||))

-- | Distribute requests amongst N slave circuits
roundrobin ::
  forall n dom addressWidth a.
  ( KnownNat n
  , HiddenClockResetEnable dom
  , KnownNat addressWidth
  , KnownNat (BitSize a)
  , NFDataX a
  , 1 <= n
  ) =>
  Circuit
    (Wishbone dom 'Standard addressWidth a)
    (Vec n (Wishbone dom 'Standard addressWidth a))
roundrobin = Circuit $ \(m2s, s2ms) -> B.first head $ fn (singleton m2s, s2ms)
 where
  Circuit fn = sharedBus selectFn
  selectFn (unbundle -> (mIdx, sIdx, _)) =
    liftA2 (,) mIdx (satSucc SatWrap <$> sIdx)

{- | General-purpose shared-bus with N masters and M slaves.

  A selector signal is used to compute the next M-S pair.
-}
sharedBus ::
  forall n m dom addressWidth a.
  ( KnownNat n
  , KnownNat m
  , HiddenClockResetEnable dom
  , KnownNat addressWidth
  , KnownNat (BitSize a)
  , NFDataX a
  ) =>
  -- | Funcion to select which M-S pair should be connected next.
  ( Signal
      dom
      ( Index n
      , Index m
      , Vec n (WishboneM2S addressWidth (BitSize a `DivRU` 8) a)
      ) ->
    Signal dom (Index n, Index m)
  ) ->
  Circuit
    (Vec n (Wishbone dom 'Standard addressWidth a))
    (Vec m (Wishbone dom 'Standard addressWidth a))
sharedBus selectFn = Circuit go
 where
  go (bundle -> m2ss0, bundle -> s2ms0) = (unbundle s2ms1, unbundle m2ss1)
   where
    mIdx0 = regEn (0 :: Index n) acceptIds mIdx1
    sIdx0 = regEn (0 :: Index m) acceptIds sIdx1

    (mIdx1, sIdx1) = unbundle $ selectFn (liftA3 (,,) mIdx0 sIdx0 m2ss0)

    m2s = liftA2 (!!) m2ss0 mIdx0
    s2m = liftA2 (!!) s2ms0 sIdx0

    acceptIds = (not . busCycle <$> m2s) .&&. (not . lock <$> m2s)

    m2ss1 = liftA3 replace sIdx0 m2s $ pure (repeat emptyWishboneM2S)
    s2ms1 = liftA3 replace mIdx0 s2m $ pure (repeat emptyWishboneS2M)

-- | Crossbar-Switch circuit, allowing to dynamically route N masters to N slaves
crossbarSwitch ::
  forall n m dom addressWidth a.
  ( KnownNat n
  , KnownNat m
  , KnownDomain dom
  , KnownNat addressWidth
  , NFDataX a
  , KnownNat (BitSize a)
  ) =>
  Circuit
    ( CSignal dom (Vec n (Index m)) -- route
    , Vec n (Wishbone dom 'Standard addressWidth a) -- masters
    )
    (Vec m (Wishbone dom 'Standard addressWidth a)) -- slaves
crossbarSwitch = Circuit go
 where
  go ((route, bundle -> m2ss0), bundle -> s2ms0) =
    ((pure (), unbundle s2ms1), unbundle m2ss1)
   where
    m2ss1 = scatter @_ @_ @_ @_ @0 (repeat emptyWishboneM2S) <$> route <*> m2ss0
    s2ms1 = gather <$> s2ms0 <*> route

-- | State for making guaranteeing correct timing of responses in 'memoryWb'
data MemoryDelayState = Wait | AckRead
  deriving (Generic, NFDataX)

{- | Memory component circuit using a specific RAM function

  This circuit uses 'Standard' mode and only supports the classic cycle type.
  Because of this, the data rate is limited by the one-cycle delay of the RAM
  function when reading and the inserted stall-cycle.

  The data rate could be increased by using registered feedback cycles or
  by using a pipelined circuit which would eliminate one wait cycle.

  Since the underlying block RAM operates on values of @a@ directly, the only
  accepted bus selector value is 'maxBound'. All other bus selector values
  will cause an ERR response.

  TODO create pipelined memory circuit
-}
memoryWb ::
  forall dom a addressWidth.
  ( BitPack a
  , NFDataX a
  , KnownDomain dom
  , KnownNat addressWidth
  , HiddenClockResetEnable dom
  , Default a
  ) =>
  ( Signal dom (BitVector addressWidth) ->
    Signal dom (Maybe (BitVector addressWidth, a)) ->
    Signal dom a
  ) ->
  Circuit (Wishbone dom 'Standard addressWidth a) ()
memoryWb ram = Circuit go
 where
  go (m2s, ()) = (mux inCycle s2m1 (pure emptyWishboneS2M), ())
   where
    inCycle = (busCycle <$> m2s) .&&. (strobe <$> m2s)
    s2m0 = reply m2s
    s2m1 = mealy delayControls Wait (bundle (m2s, s2m0))

  delayControls st (m2s, s2m)
    -- no need to delay non-ACK responses
    | err s2m || retry s2m = (Wait, s2m)
    -- write requests can be ACKed directly
    | Wait <- st, writeEnable m2s = (Wait, emptyWishboneS2M{acknowledge = True})
    -- read ACKs need to be delayed one cycle
    | Wait <- st, otherwise = (AckRead, emptyWishboneS2M)
    | AckRead <- st = (Wait, s2m)

  reply ::
    Signal dom (WishboneM2S addressWidth (BitSize a `DivRU` 8) a) ->
    Signal dom (WishboneS2M a)
  reply request = do
    ack <- (writeAck .||. readAck) .&&. (not <$> isError) .&&. requestValid
    errVal <- isError
    val <- readValue
    pure $ (emptyWishboneS2M @a){acknowledge = ack, err = errVal, readData = val}
   where
    addr1 = addr <$> request
    writeData1 = writeData <$> request
    writeAck = (\WishboneM2S{..} -> writeEnable && strobe && busCycle) <$> request
    write =
      mux
        writeAck
        (Just <$> ((,) <$> addr1 <*> writeData1))
        (pure Nothing)

    readAck = (\WishboneM2S{..} -> writeEnable && strobe && busCycle) <$> request

    requestValid = (busCycle <$> request) .&&. (strobe <$> request)

    readValue = ram addr1 write
    isError = requestValid .&&. (/= maxBound) <$> (busSelect <$> request)
