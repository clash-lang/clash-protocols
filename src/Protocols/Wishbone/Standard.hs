{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

-- | Circuits and utils for working with Standard mode wishbone circuits.
module Protocols.Wishbone.Standard where

import Clash.Prelude
import qualified Data.Bifunctor as B
import Protocols
import Protocols.Internal
import Protocols.Wishbone
import Prelude hiding (head, not, repeat, (!!), (&&))

-- | Distribute requests amongst N slave circuits
roundrobin ::
  forall n dom addressWidth a.
  ( KnownNat n,
    HiddenClockResetEnable dom,
    KnownNat addressWidth,
    KnownNat (BitSize a),
    NFDataX a,
    1 <= n
  ) =>
  Circuit
    (Wishbone dom 'Standard addressWidth a)
    (Vec n (Wishbone dom 'Standard addressWidth a))
roundrobin = Circuit $ \(m2s, s2ms) -> B.first head $ fn (singleton m2s, s2ms)
  where
    Circuit fn = sharedBus selectFn
    selectFn (unbundle -> (mIdx, sIdx, _)) =
      liftA2 (,) mIdx (satSucc SatWrap <$> sIdx)

-- | General-purpose shared-bus with N masters and M slaves.
--
--   A selector signal is used to compute the next M-S pair.
sharedBus ::
  forall n m dom addressWidth a.
  ( KnownNat n,
    KnownNat m,
    HiddenClockResetEnable dom,
    KnownNat addressWidth,
    KnownNat (BitSize a),
    NFDataX a
  ) =>
  -- | Funcion to select which M-S pair should be connected next.
  ( Signal
      dom
      ( Index n,
        Index m,
        Vec n (WishboneM2S addressWidth (BitSize a `DivRU` 8) a)
      ) ->
    Signal dom (Index n, Index m)
  ) ->
  Circuit
    (Vec n (Wishbone dom 'Standard addressWidth a))
    (Vec m (Wishbone dom 'Standard addressWidth a))
sharedBus selectFn = Circuit go
  where
    go (bundle -> m2ss, bundle -> s2ms) = (unbundle s2ms', unbundle m2ss')
      where
        mIdx = regEn (0 :: Index n) acceptIds mIdx'
        sIdx = regEn (0 :: Index m) acceptIds sIdx'

        (mIdx', sIdx') = unbundle $ selectFn (liftA3 (,,) mIdx sIdx m2ss)

        m2s = liftA2 (!!) m2ss mIdx
        s2m = liftA2 (!!) s2ms sIdx

        acceptIds = (not . busCycle <$> m2s) .&&. (not . lock <$> m2s)

        m2ss' = liftA3 replace sIdx m2s $ pure (repeat emptyWishboneM2S)
        s2ms' = liftA3 replace mIdx s2m $ pure (repeat emptyWishboneS2M)

-- | Crossbar-Switch circuit, allowing to dynamically route N masters to N slaves
crossbarSwitch ::
  forall n m dom addressWidth a.
  ( KnownNat n,
    KnownNat m,
    KnownDomain dom,
    KnownNat addressWidth,
    NFDataX a,
    KnownNat (BitSize a)
  ) =>
  Circuit
    ( CSignal dom (Vec n (Index m)), -- route
      Vec n (Wishbone dom 'Standard addressWidth a) -- masters
    )
    (Vec m (Wishbone dom 'Standard addressWidth a)) -- slaves
crossbarSwitch = Circuit go
  where
    go ((CSignal route, bundle -> m2ss), bundle -> s2ms) =
      ((CSignal (pure ()), unbundle s2ms'), unbundle m2ss')
      where
        m2ss' = scatter @_ @_ @_ @_ @0 (repeat emptyWishboneM2S) <$> route <*> m2ss
        s2ms' = gather <$> s2ms <*> route

-- | Memory component circuit using a specific RAM function
--
--   This circuit uses 'Standard' mode and only supports the classic cycle type.
--   Because of this, the data rate is limited by the one-cycle delay of the RAM
--   function when reading and the inserted stall-cycle.
--
--   The data rate could be increased by using registered feedback cycles or
--   by using a pipelined circuit which would eliminate one wait cycle.
--
--   TODO create pipelined memory circuit
memoryWb ::
  forall dom a addressWidth.
  ( BitPack a,
    NFDataX a,
    KnownDomain dom,
    KnownNat addressWidth,
    HiddenClockResetEnable dom,
    Default a
  ) =>
  (Signal dom (BitVector addressWidth) -> Signal dom (Maybe (BitVector addressWidth, a)) -> Signal dom a) ->
  Circuit (Wishbone dom 'Standard addressWidth a) ()
memoryWb ram = Circuit go
  where
    go (m2s, ()) = (reply m2s, ())

    reply ::
      Signal dom (WishboneM2S addressWidth (BitSize a `DivRU` 8) a) ->
      Signal dom (WishboneS2M a)
    reply request = do
      ack <- writeAck .||. readAck
      val <- readValue
      pure $ (emptyWishboneS2M @a) {acknowledge = ack, readData = val}
      where
        addr' = addr <$> request
        writeData' = writeData <$> request
        isWriteRequest = (\WishboneM2S {..} -> writeEnable && strobe && busCycle) <$> request
        write =
          mux
            isWriteRequest
            (Just <$> ((,) <$> addr' <*> writeData'))
            (pure Nothing)

        writeAck = isRising False isWriteRequest

        isReadRequest = (\WishboneM2S {..} -> writeEnable && strobe && busCycle) <$> request
        justReadRequest = isRising False isReadRequest

        readAck = register False justReadRequest

        readValue = ram addr' write
