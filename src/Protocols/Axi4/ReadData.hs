{-|
Defines ReadData channel of full AXI4 protocol with port names corresponding
to the AXI4 specification.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-missing-fields #-}

module Protocols.Axi4.ReadData
  ( M2S_ReadData(..)
  , S2M_ReadData(..)
  , Axi4ReadData
  , mapFull

    -- * Operations on Df like protocols
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
  , roundrobinCollect
  , registerFwd
  , registerBwd

  ) where

-- base
import Data.Coerce (coerce)
import Data.Bifunctor (Bifunctor)
import Data.Kind (Type)
import GHC.Generics (Generic)
import Data.Proxy
import           Prelude hiding
  ((!!), map, zip, zipWith, filter, fst, snd, either, const, pure)

-- clash-prelude
import qualified Clash.Prelude as C

-- me
import Protocols.Axi4.Common
import Protocols.Internal
import Protocols.DfLike (DfLike)
import qualified Protocols.DfLike as DfLike

-- | AXI4 Read Data channel protocol
data Axi4ReadData
  (dom :: C.Domain)
  (kr :: KeepResponse)
  (iw :: IdWidth)
  (userType :: Type)
  (dataType :: Type)

instance Protocol (Axi4ReadData dom kr iw userType dataType) where
  type Fwd (Axi4ReadData dom kr iw userType dataType) =
    C.Signal dom (S2M_ReadData kr iw userType dataType)
  type Bwd (Axi4ReadData dom kr iw userType dataType) =
    C.Signal dom M2S_ReadData

instance Backpressure (Axi4ReadData dom kr iw userType dataType) where
  boolsToBwd _ = C.fromList_lazy . coerce

instance DfLike dom (Axi4ReadData dom kr iw userType) dataType where
  type Data (Axi4ReadData dom kr iw userType) dataType =
    S2M_ReadData kr iw userType dataType

  type Payload dataType = dataType

  type Ack (Axi4ReadData dom kr iw userType) dataType  =
    M2S_ReadData

  getPayload _ (S2M_ReadData{_rdata}) = Just _rdata
  getPayload _ S2M_NoReadData = Nothing
  {-# INLINE getPayload #-}

  setPayload _ _ dat (Just b) = dat{_rdata=b}
  setPayload _ dfB _ Nothing = DfLike.noData dfB
  {-# INLINE setPayload #-}

  noData _ = S2M_NoReadData
  {-# INLINE noData #-}

  boolToAck _ = coerce
  {-# INLINE boolToAck #-}

  ackToBool _ = coerce
  {-# INLINE ackToBool #-}

instance (C.KnownDomain dom, C.NFDataX dataType, C.ShowX dataType, Show dataType) =>
  Simulate (Axi4ReadData dom kr iw userType dataType) where

  type SimulateType (Axi4ReadData dom kr iw userType dataType) =
    [S2M_ReadData kr iw userType dataType]

  type ExpectType (Axi4ReadData dom kr iw userType dataType) =
    [S2M_ReadData kr iw userType dataType]

  type SimulateChannels (Axi4ReadData dom kr iw userType dataType) = 1

  toSimulateType _ = id
  fromSimulateType _ = id

  driveC = DfLike.drive Proxy
  sampleC = DfLike.sample Proxy
  stallC conf (C.head -> (stallAck, stalls)) =
    DfLike.stall Proxy conf stallAck stalls

-- | See Table A2-6 "Read data channel signals"
data S2M_ReadData
  (kr :: KeepResponse)
  (iw :: IdWidth)
  (userType :: Type)
  (dataType :: Type)
  = S2M_NoReadData
  | S2M_ReadData
    { -- | Read address id*
      _rid :: !(C.BitVector (Width iw))

    , -- | Read data
      _rdata :: !dataType

      -- | Read response
    , _rresp :: !(ResponseType kr)

      -- | Read last
    , _rlast :: !Bool

      -- | User data
    , _ruser :: !userType
    }
  deriving (Generic)

-- | See Table A2-6 "Read data channel signals"
newtype M2S_ReadData = M2S_ReadData { _rready :: Bool }
  deriving (Show, Generic, C.NFDataX)

deriving instance
  ( C.NFDataX userType
  , C.NFDataX dataType
  , C.NFDataX (ResponseType kr)
  , C.KnownNat (Width iw)
  ) =>
  C.NFDataX (S2M_ReadData kr iw userType dataType)

deriving instance
  ( C.KnownNat (Width iw)
  , Show userType
  , Show dataType
  , Show (ResponseType kr) ) =>
  Show (S2M_ReadData kr iw userType dataType)


-- | Like 'P.map', but over the data in the AXI4 read channel.
map :: (a -> b) -> Circuit
  (Axi4ReadData dom kr iw userType a)
  (Axi4ReadData dom kr iw userType b)
map = DfLike.map Proxy Proxy

-- | Like 'bimap', but over the data in the AXI4 read channel.
bimap :: Bifunctor p => (a -> b) -> (c -> d) -> Circuit
  (Axi4ReadData dom kr iw userType (p a c))
  (Axi4ReadData dom kr iw userType (p b d))
bimap = DfLike.bimap

-- | Like 'P.fst', but over the data in the AXI4 read channel.
fst :: Circuit
  (Axi4ReadData dom kr iw userType (a, b))
  (Axi4ReadData dom kr iw userType a)
fst = DfLike.fst

-- | Like 'P.snd', but over the data in the AXI4 read channel.
snd :: Circuit
  (Axi4ReadData dom kr iw userType (a, b))
  (Axi4ReadData dom kr iw userType b)
snd = DfLike.snd

-- | Like 'Data.Bifunctor.first', but over the data in the AXI4 read channel.
first :: Bifunctor p => (a -> b) -> Circuit
  (Axi4ReadData dom kr iw userType (p a c))
  (Axi4ReadData dom kr iw userType (p b c))
first = DfLike.first

-- | Like 'Data.Bifunctor.second', but over the data in the AXI4 read channel.
second :: Bifunctor p => (b -> c) -> Circuit
  (Axi4ReadData dom kr iw userType (p a b))
  (Axi4ReadData dom kr iw userType (p a c))
second = DfLike.second

-- | Acknowledge but ignore data from LHS protocol. Send a static value /b/.
const :: C.HiddenReset dom => S2M_ReadData kr iw userType b -> Circuit
  (Axi4ReadData dom kr iw userType a)
  (Axi4ReadData dom kr iw userType b)
const = DfLike.const Proxy Proxy

-- | Drive a constant value composed of /a/.
pure :: S2M_ReadData kr iw userType a -> Circuit
  ()
  (Axi4ReadData dom kr iw userType a)
pure = DfLike.pure Proxy

-- | Drive a constant value composed of /a/.
void :: C.HiddenReset dom => Circuit (Axi4ReadData dom kr iw userType a) ()
void = DfLike.void Proxy

-- | Like 'Data.Maybe.catMaybes', but over a AXI4 read data stream.
catMaybes :: Circuit
  (Axi4ReadData dom kr iw userType (Maybe a))
  (Axi4ReadData dom kr iw userType a)
catMaybes = DfLike.catMaybes Proxy Proxy

-- | Like 'Data.Maybe.mapMaybe', but over payload (/a/) of a AXI4 read data stream.
mapMaybe :: (a -> Maybe b) -> Circuit
  (Axi4ReadData dom kr iw userType a)
  (Axi4ReadData dom kr iw userType b)
mapMaybe = DfLike.mapMaybe

-- | Like 'P.filter', but over a AXI4 read data stream.
filter :: forall dom a kr iw userType. (a -> Bool) -> Circuit
  (Axi4ReadData dom kr iw userType a)
  (Axi4ReadData dom kr iw userType a)
filter = DfLike.filter Proxy

-- | Like 'Data.Either.Combinators.mapLeft', but over the data in the AXI4 read channel.
mapLeft :: (a -> b) -> Circuit
  (Axi4ReadData dom kr iw userType (Either a c))
  (Axi4ReadData dom kr iw userType (Either b c))
mapLeft = DfLike.mapLeft

-- | Like 'Data.Either.Combinators.mapRight', but over the data in the AXI4 read channel.
mapRight :: (b -> c) -> Circuit
  (Axi4ReadData dom kr iw userType (Either a b))
  (Axi4ReadData dom kr iw userType (Either a c))
mapRight = DfLike.mapRight

-- | Like 'Data.Either.either', but over a AXI4 read data stream.
either :: (a -> c) -> (b -> c) -> Circuit
  (Axi4ReadData dom kr iw userType (Either a b))
  (Axi4ReadData dom kr iw userType c)
either = DfLike.either


-- | Like 'P.zipWith', but over two AXI4 read data streams.
zipWith ::
  forall dom a b c kr iw userType .
  (a -> b -> c) ->
  Circuit
    (Axi4ReadData dom kr iw userType a, Axi4ReadData dom kr iw userType b)
    (Axi4ReadData dom kr iw userType c)
zipWith = DfLike.zipWith Proxy Proxy Proxy

-- | Like 'P.zip', but over two AXI4 read data streams.
zip :: forall a b dom kr iw userType . Circuit
  (Axi4ReadData dom kr iw userType a, Axi4ReadData dom kr iw userType b)
  (Axi4ReadData dom kr iw userType (a, b))
zip = DfLike.zip Proxy Proxy Proxy

-- | Like 'P.partition', but over AXI4 read data streams
partition :: forall dom a kr iw userType. (a -> Bool) -> Circuit
  (Axi4ReadData dom kr iw userType a)
  (Axi4ReadData dom kr iw userType a, Axi4ReadData dom kr iw userType a)
partition = DfLike.partition Proxy


-- | Route a AXI4 read data stream to another corresponding to the index
route ::
  forall n dom a kr iw userType .
  C.KnownNat n => Circuit
    (Axi4ReadData dom kr iw userType (C.Index n, a))
    (C.Vec n (Axi4ReadData dom kr iw userType a))
route = DfLike.route Proxy Proxy


-- | Select data from the channel indicated by the AXI4 read data stream carrying
-- @Index n@.
select ::
  forall n dom a kr iw userType .
  C.KnownNat n =>
  Circuit
    (C.Vec n (Axi4ReadData dom kr iw userType a), Axi4ReadData dom kr iw userType (C.Index n))
    (Axi4ReadData dom kr iw userType a)
select = DfLike.select



-- | Select /selectN/ samples from channel /n/.
selectN ::
  forall n selectN dom a kr iw userType.
  ( C.HiddenClockResetEnable dom
  , C.KnownNat selectN
  , C.KnownNat n
  ) =>
  Circuit
    (C.Vec n (Axi4ReadData dom kr iw userType a), Axi4ReadData dom kr iw userType (C.Index n, C.Index selectN))
    (Axi4ReadData dom kr iw userType a)
selectN = DfLike.selectN Proxy Proxy

-- | Selects samples from channel /n/ until the predicate holds. The cycle in
-- which the predicate turns true is included.
selectUntil ::
  forall n dom a kr iw userType .
  C.KnownNat n =>
  (a -> Bool) ->
  Circuit
    (C.Vec n (Axi4ReadData dom kr iw userType a), Axi4ReadData dom kr iw userType (C.Index n))
    (Axi4ReadData dom kr iw userType a)
selectUntil = DfLike.selectUntil Proxy Proxy

-- | Copy data of a single AXI4 read data stream to multiple. LHS will only receive
-- an acknowledgement when all RHS receivers have acknowledged data.
fanout ::
  forall n dom a kr iw userType .
  (C.KnownNat n, C.HiddenClockResetEnable dom, 1 C.<= n) =>
  Circuit
    (Axi4ReadData dom kr iw userType a)
    (C.Vec n (Axi4ReadData dom kr iw userType a))
fanout = DfLike.fanout Proxy

-- | Merge data of multiple AXI4 read data streams using a user supplied function
fanin ::
  forall n dom a kr iw userType .
  (C.KnownNat n, 1 C.<= n) =>
  (a -> a -> a) ->
  Circuit
    (C.Vec n (Axi4ReadData dom kr iw userType a))
    (Axi4ReadData dom kr iw userType a)
fanin = DfLike.fanin

-- | Merge data of multiple AXI4 read data streams using Monoid's '<>'.
mfanin ::
  forall n dom a kr iw userType .
  (C.KnownNat n, Monoid a, 1 C.<= n) =>
  Circuit
    (C.Vec n (Axi4ReadData dom kr iw userType a))
    (Axi4ReadData dom kr iw userType a)
mfanin = DfLike.mfanin

-- | Bundle a vector of AXI4 read data streams into one.
bundleVec ::
  forall n dom a kr iw userType .
  (C.KnownNat n, 1 C.<= n) =>
  Circuit
    (C.Vec n (Axi4ReadData dom kr iw userType a))
    (Axi4ReadData dom kr iw userType (C.Vec n a))
bundleVec = DfLike.bundleVec Proxy Proxy

-- | Split up a AXI4 read data stream of a vector into multiple independent AXI4 read data streams.
unbundleVec ::
  forall n dom a kr iw userType .
  (C.KnownNat n, C.NFDataX a, C.HiddenClockResetEnable dom, 1 C.<= n) =>
  Circuit
    (Axi4ReadData dom kr iw userType (C.Vec n a))
    (C.Vec n (Axi4ReadData dom kr iw userType a))
unbundleVec = DfLike.unbundleVec Proxy Proxy

-- | Distribute data across multiple components on the RHS. Useful if you want
-- to parallelize a workload across multiple (slow) workers. For optimal
-- throughput, you should make sure workers can accept data every /n/ cycles.
roundrobin ::
  forall n dom a kr iw userType .
  (C.KnownNat n, C.HiddenClockResetEnable dom, 1 C.<= n) =>
  Circuit
    (Axi4ReadData dom kr iw userType a)
    (C.Vec n (Axi4ReadData dom kr iw userType a))
roundrobin = DfLike.roundrobin Proxy

-- | Opposite of 'roundrobin'. Useful to collect data from workers that only
-- produce a result with an interval of /n/ cycles.
roundrobinCollect ::
  forall n dom a kr iw userType .
  (C.KnownNat n, C.HiddenClockResetEnable dom, 1 C.<= n) =>
  DfLike.CollectMode ->
  Circuit
    (C.Vec n (Axi4ReadData dom kr iw userType a))
    (Axi4ReadData dom kr iw userType a)
roundrobinCollect = DfLike.roundrobinCollect Proxy

-- | Place register on /forward/ part of a circuit.
registerFwd ::
  forall dom a kr iw userType .
  ( C.NFDataX a
  , C.HiddenClockResetEnable dom
  , C.NFDataX (ResponseType kr)
  , C.NFDataX userType
  , C.KnownNat (Width iw)
  ) =>
  Circuit
    (Axi4ReadData dom kr iw userType a)
    (Axi4ReadData dom kr iw userType a)
registerFwd = DfLike.registerFwd Proxy

-- | Place register on /backward/ part of a circuit. This is implemented using a
-- in-logic two-element shift register.
registerBwd ::
  ( C.NFDataX a
  , C.HiddenClockResetEnable dom
  , C.NFDataX (ResponseType kr)
  , C.NFDataX userType
  , C.KnownNat (Width iw)
  ) =>
  Circuit
    (Axi4ReadData dom kr iw userType a)
    (Axi4ReadData dom kr iw userType a)
registerBwd = DfLike.registerBwd Proxy


-- | Circuit that transforms the LHS version of the 'Axi4ReadData' protocol to a
-- version using different parameters according to two functions
-- that can transform the data and ack signal accordingly to and from the other protocol.
mapFull ::
  forall dom
    kr1 iw1 userType1 dataType1
    kr2 iw2 userType2 dataType2 .
  (S2M_ReadData kr1 iw1 userType1 dataType1 -> S2M_ReadData kr2 iw2 userType2 dataType2) ->
  (M2S_ReadData -> M2S_ReadData) ->
  Circuit
    (Axi4ReadData dom kr1 iw1 userType1 dataType1)
    (Axi4ReadData dom kr2 iw2 userType2 dataType2)
mapFull = DfLike.mapDfLike Proxy Proxy

