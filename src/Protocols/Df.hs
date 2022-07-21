{-|
Defines data structures and operators to create a Dataflow protocol that only
carries data, no metadata. For documentation see:

  * 'Protocols.Circuit'
  * 'Protocols.Df.Df'

-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Protocols.Df
  ( -- * Types
    Df, Data(..)

    -- * Operations on Df protocol
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
  , CollectMode(..)
  , roundrobinCollect
  , registerFwd
  , registerBwd
  , fifo

    -- * Simulation functions
  , drive
  , stall
  , sample
  , simulate

    -- * Internals
  , forceResetSanity
  , dataToMaybe
  , maybeToData
  ) where

-- base
import           Control.Applicative (Alternative((<|>)), Applicative(liftA2))
import           Control.DeepSeq (NFData)
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)
import           Prelude hiding
  ((!!), map, zip, zipWith, filter, fst, snd, either, const, pure)


import qualified Data.Bifunctor as B
import           Data.Bool (bool)
import           Data.Coerce (coerce)
import           Data.Kind (Type)
import           Data.List ((\\))
import qualified Data.List.NonEmpty
import qualified Data.Maybe as Maybe
import           Data.Proxy
import qualified Prelude as P

-- clash-prelude
import           Clash.Prelude (type (<=))
import           Clash.Signal.Internal (Signal(..))
import qualified Clash.Prelude as C
import qualified Clash.Explicit.Prelude as CE

-- me
import           Protocols.Internal

-- $setup
-- >>> import Protocols
-- >>> import Clash.Prelude (Vec(..))
-- >>> import qualified Prelude as P
-- >>> import qualified Data.Bifunctor as B

-- | Like 'Protocols.Df', but without metadata.
--
-- __N.B.__: For performance reasons 'Protocols.Data' is strict on
-- its data field. That is, if 'Protocols.Data' is evaluated to WHNF,
-- its fields will be evaluated to WHNF too.
data Df (dom :: C.Domain) (a :: Type)

instance Protocol (Df dom a) where
  -- | Forward part of simple dataflow: @Signal dom (Data meta a)@
  type Fwd (Df dom a) = Signal dom (Data a)

  -- | Backward part of simple dataflow: @Signal dom Bool@
  type Bwd (Df dom a) = Signal dom Ack


instance Backpressure (Df dom a) where
  boolsToBwd _ = C.fromList_lazy . coerce

-- | Data sent over forward channel of 'Df'. Note that this data type is strict
-- on its data field.
data Data a
  -- | Send no data
  = NoData
  -- | Send /a/
  | Data !a
  deriving (Functor, Generic, C.NFDataX, C.ShowX, Eq, NFData, Show, C.Bundle)

instance Applicative Data where
  pure = Data

  liftA2 f (Data a) (Data b) = Data (f a b)
  liftA2 _ _        _        = NoData

instance Alternative Data where
  empty = NoData

  Data a <|> _ = Data a
  _      <|> b = b

instance Monad Data where
  (>>=) :: Data a -> (a -> Data b) -> Data b
  NoData >>= _f = NoData
  Data a >>=  f = f a

-- | Convert 'Data' to 'Maybe'. Produces 'Just' on 'Data', 'Nothing' on 'NoData'.
dataToMaybe :: Data a -> Maybe a
dataToMaybe NoData = Nothing
dataToMaybe (Data a) = Just a

-- | Convert 'Maybe' to 'Data'. Produces 'Data' on 'Just', 'NoData' on 'Nothing'.
maybeToData :: Maybe a -> Data a
maybeToData Nothing = NoData
maybeToData (Just a) = Data a

instance (C.KnownDomain dom, C.NFDataX a, C.ShowX a, Show a) => Simulate (Df dom a) where
  type SimulateFwdType (Df dom a) = [Data a]
  type SimulateBwdType (Df dom a) = [Ack]
  type SimulateChannels (Df dom a) = 1

  simToSigFwd _ = C.fromList_lazy
  simToSigBwd _ = C.fromList_lazy
  sigToSimFwd _ = C.sample_lazy
  sigToSimBwd _ = C.sample_lazy

  stallC conf (C.head -> (stallAck, stalls)) = stall conf stallAck stalls

instance (C.KnownDomain dom, C.NFDataX a, C.ShowX a, Show a) => Drivable (Df dom a) where
  type ExpectType (Df dom a) = [a]

  toSimulateType Proxy = P.map Data
  fromSimulateType Proxy = Maybe.mapMaybe dataToMaybe

  driveC conf vals = drive conf (dataToMaybe <$> vals)
  sampleC conf ckt = maybeToData <$> sample conf ckt


-- | Force a /nack/ on the backward channel and /no data/ on the forward
-- channel if reset is asserted.
forceResetSanity :: forall dom a. C.HiddenClockResetEnable dom => Circuit (Df dom a) (Df dom a)
forceResetSanity
  = Circuit (\(fwd, bwd) -> C.unbundle . fmap f . C.bundle $ (rstLow, fwd, bwd))
 where
  f (True,  _,   _  ) = (Ack False, NoData)
  f (False, fwd, bwd) = (bwd, fwd)
  rstLow = C.unsafeToHighPolarity C.hasReset

-- | Like 'P.map', but over payload (/a/) of a Df stream.
map :: (a -> b) -> Circuit (Df dom a) (Df dom b)
map f = Circuit (uncurry go)
 where
  go fwd bwd =
    ( bwd
    , fmap f <$> fwd )

-- | Like 'P.map', but over payload (/a/) of a Df stream.
bimap :: B.Bifunctor p => (a -> b) -> (c -> d) -> Circuit (Df dom (p a c)) (Df dom (p b d))
bimap f g = map (B.bimap f g)

-- | Like 'P.fst', but over payload of a Df stream.
fst :: Circuit (Df dom (a, b)) (Df dom a)
fst = map P.fst

-- | Like 'P.snd', but over payload of a Df stream.
snd :: Circuit (Df dom (a, b)) (Df dom b)
snd = map P.snd

-- | Like 'Data.Bifunctor.first', but over payload of a Df stream.
first :: B.Bifunctor p => (a -> b) -> Circuit (Df dom (p a c)) (Df dom (p b c))
first f = map (B.first f)

-- | Like 'Data.Bifunctor.second', but over payload of a Df stream.
second :: B.Bifunctor p => (b -> c) -> Circuit (Df dom (p a b)) (Df dom (p a c))
second f = map (B.second f)

-- | Acknowledge but ignore data from LHS protocol. Send a static value /b/.
const :: C.HiddenReset dom => b -> Circuit (Df dom a) (Df dom b)
const b = Circuit (P.const (Ack <$> C.unsafeToLowPolarity C.hasReset, P.pure (Data b)))

-- | Drive a constant value composed of /a/.
pure :: a -> Circuit () (Df dom a)
pure a = Circuit (P.const ((), P.pure (Data a)))

-- | Drive a constant value composed of /a/.
void :: C.HiddenReset dom => Circuit (Df dom a) ()
void = Circuit (P.const (Ack <$> C.unsafeToLowPolarity C.hasReset, ()))

-- | Like 'Data.Maybe.catMaybes', but over a Df stream.
--
-- Example:
--
-- >>> take 2 (simulateCS (catMaybes @C.System @Int) [Nothing, Just 1, Nothing, Just 3])
-- [1,3]
--
catMaybes :: Circuit (Df dom (Maybe a)) (Df dom a)
catMaybes = Circuit (C.unbundle . fmap go . C.bundle)
 where
  go (NoData, _) = (Ack False, NoData)
  go (Data Nothing, _) = (Ack True, NoData)
  go (Data (Just a), ack) = (ack, Data a)

-- | Like 'Data.Maybe.mapMaybe', but over payload (/a/) of a Df stream.
mapMaybe :: (a -> Maybe b) -> Circuit (Df dom a) (Df dom b)
mapMaybe f = map f |> catMaybes

-- | Like 'P.filter', but over a 'Df' stream.
--
-- Example:
--
-- >>> take 3 (simulateCS (filter @C.System @Int (>5)) [1, 5, 7, 10, 3, 11])
-- [7,10,11]
--
filter :: forall dom a. (a -> Bool) -> Circuit (Df dom a) (Df dom a)
filter f = Circuit (C.unbundle . fmap go . C.bundle)
 where
  go (NoData, _) = (Ack False, NoData)
  go (Data d, ack)
    | f d = (ack, Data d)
    | otherwise = (Ack True, NoData)

-- | Like 'Data.Either.Combinators.mapLeft', but over payload of a 'Df' stream.
mapLeft :: (a -> b) -> Circuit (Df dom (Either a c)) (Df dom (Either b c))
mapLeft = first

-- | Like 'Data.Either.Combinators.mapRight', but over payload of a 'Df' stream.
mapRight :: (b -> c) -> Circuit (Df dom (Either a b)) (Df dom (Either a c))
mapRight = second

-- | Like 'Data.Either.either', but over a 'Df' stream.
either :: (a -> c) -> (b -> c) -> Circuit (Df dom (Either a b)) (Df dom c)
either f g = map (P.either f g)

-- | Like 'P.zipWith', but over two 'Df' streams.
--
-- Example:
--
-- >>> take 3 (simulateCS (zipWith @C.System @Int (+)) ([1, 3, 5], [2, 4, 7]))
-- [3,7,12]
--
zipWith ::
  forall dom a b c.
  (a -> b -> c) ->
  Circuit
    (Df dom a, Df dom b)
    (Df dom c)
zipWith f
  = Circuit (B.first C.unbundle . C.unbundle . fmap go . C.bundle . B.first C.bundle)
 where
  go ((Data a, Data b), ack) = ((ack, ack), Data (f a b))
  go _ = ((Ack False, Ack False), NoData)

-- | Like 'P.zip', but over two 'Df' streams.
zip :: forall a b dom. Circuit (Df dom a, Df dom b) (Df dom (a, b))
zip = zipWith (,)

-- | Like 'P.partition', but over 'Df' streams
--
-- Example:
--
-- >>> let input = [1, 3, 5, 7, 9, 2, 11]
-- >>> let output = simulateCS (partition @C.System @Int (>5)) input
-- >>> B.bimap (take 3) (take 4) output
-- ([7,9,11],[1,3,5,2])
--
partition :: forall dom a. (a -> Bool) -> Circuit (Df dom a) (Df dom a, Df dom a)
partition f
  = Circuit (B.second C.unbundle . C.unbundle . fmap go . C.bundle . B.second C.bundle)
 where
  go (Data a, (ackT, ackF))
    | f a = (ackT, (Data a, NoData))
    | otherwise = (ackF, (NoData, Data a))
  go _ = (Ack False, (NoData, NoData))

-- | Route a 'Df' stream to another corresponding to the index
--
-- Example:
--
#if MIN_VERSION_clash_prelude(1,6,0)
-- >>> let input = [(0, 3), (0, 5), (1, 7), (2, 13), (1, 11), (2, 1)]
-- >>> let output = simulateCS (route @3 @C.System @Int) input
-- >>> fmap (take 2) output
-- [3,5] :> [7,11] :> [13,1] :> Nil
#else
-- >>> let input = [(0, 3), (0, 5), (1, 7), (2, 13), (1, 11), (2, 1)]
-- >>> let output = simulateCS (route @3 @C.System @Int) input
-- >>> fmap (take 2) output
-- <[3,5],[7,11],[13,1]>
#endif
--
route ::
  forall n dom a. C.KnownNat n =>
  Circuit (Df dom (C.Index n, a)) (C.Vec n (Df dom a))
route
  = Circuit (B.second C.unbundle . C.unbundle . fmap go . C.bundle . B.second C.bundle)
 where
  -- go :: (Data (C.Index n, a), C.Vec n (Ack a)) -> (Ack (C.Index n, a), C.Vec n (Data a))
  go (Data (i, a), acks) =
    ( acks C.!! i
    , C.replace i (Data a) (C.repeat NoData) )
  go _ =
    (Ack False, C.repeat NoData)

-- | Select data from the channel indicated by the 'Df' stream carrying
-- @Index n@.
--
-- Example:
--
-- >>> let indices = [1, 1, 2, 0, 2]
-- >>> let dats = [8] :> [5, 7] :> [9, 1] :> Nil
-- >>> let output = simulateCS (select @3 @C.System @Int) (dats, indices)
-- >>> take 5 output
-- [5,7,9,8,1]
--
select ::
  forall n dom a.
  C.KnownNat n =>
  Circuit (C.Vec n (Df dom a), Df dom (C.Index n)) (Df dom a)
select = selectUntil (P.const True)

-- | Select /selectN/ samples from channel /n/.
--
-- Example:
--
-- >>> let indices = [(0, 2), (1, 3), (0, 2)]
-- >>> let dats = [10, 20, 30, 40] :> [11, 22, 33] :> Nil
-- >>> let circuit = C.exposeClockResetEnable (selectN @2 @10 @C.System @Int)
-- >>> take 7 (simulateCSE circuit (dats, indices))
-- [10,20,11,22,33,30,40]
--
selectN ::
  forall n selectN dom a.
  ( C.HiddenClockResetEnable dom
  , C.KnownNat selectN
  , C.KnownNat n
  ) =>
  Circuit
    (C.Vec n (Df dom a), Df dom (C.Index n, C.Index selectN))
    (Df dom a)
selectN
  = Circuit
  ( B.first (B.first C.unbundle . C.unbundle)
  . C.mealyB go (0 :: C.Index (selectN C.+ 1))
  . B.first (C.bundle . B.first C.bundle))
 where
  go c0 ((dats, datI), Ack iAck)
    -- Select zero samples: don't send any data to RHS, acknowledge index stream
    -- but no data stream.
    | Data (_, 0) <- datI
    = (c0, ((nacks, Ack True), NoData))

    -- Acknowledge data if RHS acknowledges ours. Acknowledge index stream if
    -- we're done.
    | Data (streamI, nSelect) <- datI
    , let dat = dats C.!! streamI
    , Data d <- dat
    = let
        c1 = if iAck then succ c0 else c0
        oAckIndex = c1 == C.extend nSelect
        c2 = if oAckIndex then 0 else c1
        datAcks = C.replace streamI (Ack iAck) nacks
      in
        (c2, ( (datAcks, Ack oAckIndex)
             , Data d ))

    -- No index from LHS, nothing to do
    | otherwise
    = (c0, ((nacks, Ack False), NoData))
   where
    nacks = C.repeat (Ack False)

-- | Selects samples from channel /n/ until the predicate holds. The cycle in
-- which the predicate turns true is included.
--
-- Example:
--
-- >>> let indices = [0, 0, 1, 2]
-- >>> let channel1 = [(10, False), (20, False), (30, True), (40, True)]
-- >>> let channel2 = [(11, False), (21, True)]
-- >>> let channel3 = [(12, False), (22, False), (32, False), (42, True)]
-- >>> let dats = channel1 :> channel2 :> channel3 :> Nil
-- >>> take 10 (simulateCS (selectUntil @3 @C.System @(Int, Bool) P.snd) (dats, indices))
-- [(10,False),(20,False),(30,True),(40,True),(11,False),(21,True),(12,False),(22,False),(32,False),(42,True)]
--
selectUntil ::
  forall n dom a.
  C.KnownNat n =>
  (a -> Bool) ->
  Circuit
    (C.Vec n (Df dom a), Df dom (C.Index n))
    (Df dom a)
selectUntil f
  = Circuit
  ( B.first (B.first C.unbundle . C.unbundle)
  . C.unbundle
  . fmap go
  . C.bundle
  . B.first (C.bundle . B.first C.bundle))
 where
  nacks = C.repeat (Ack False)

  go ((dats, dat), Ack ack)
    | Data i <- dat
    , Data d <- dats C.!! i
    = ( ( C.replace i (Ack ack) nacks
        , Ack (f d && ack) )
      , Data d )
    | otherwise
    = ((nacks, Ack False), NoData)

-- | Copy data of a single 'Df' stream to multiple. LHS will only receive
-- an acknowledgement when all RHS receivers have acknowledged data.
fanout ::
  forall n dom a .
  (C.KnownNat n, C.HiddenClockResetEnable dom, 1 <= n) =>
  Circuit (Df dom a) (C.Vec n (Df dom a))
fanout = forceResetSanity |> goC
 where
  goC =
    Circuit $ \(s2r, r2s) ->
      B.second C.unbundle (C.mealyB f initState (s2r, C.bundle r2s))

  initState = C.repeat False

  f acked (dat, acks) =
    case dat of
      NoData -> (acked, (Ack False, C.repeat NoData))
      Data _ ->
        -- Data on input
        let
          -- Send data to "clients" that have not acked yet
          valids_ = C.map not acked
          dats = C.map (bool NoData dat) valids_

          -- Store new acks, send ack if all "clients" have acked
          acked1 = C.zipWith (||) acked (C.map (\(Ack a) -> a) acks)
          ack = C.fold @(n C.- 1) (&&) acked1
        in
          ( if ack then initState else acked1
          , (Ack ack, dats) )

-- | Merge data of multiple 'Df' streams using a user supplied function
fanin ::
  forall n dom a .
  (C.KnownNat n, 1 <= n) =>
  (a -> a -> a) ->
  Circuit (C.Vec n (Df dom a)) (Df dom a)
fanin f = bundleVec |> map (C.fold @(n C.- 1) f)

-- | Merge data of multiple 'Df' streams using Monoid's '<>'.
mfanin ::
  forall n dom a .
  (C.KnownNat n, Monoid a, 1 <= n) =>
  Circuit (C.Vec n (Df dom a)) (Df dom a)
mfanin = fanin (<>)

-- | Bundle a vector of 'Df' streams into one.
bundleVec ::
  forall n dom a .
  (C.KnownNat n, 1 <= n) =>
  Circuit (C.Vec n (Df dom a)) (Df dom (C.Vec n a))
bundleVec
  = Circuit (B.first C.unbundle . C.unbundle . fmap go . C.bundle . B.first C.bundle)
 where
  go (iDats0, iAck) = (C.repeat oAck, maybeToData dat)
   where
    oAck =
      bool
        (Ack False)
        (iAck)
        (Maybe.isJust dat)
    dat = traverse dataToMaybe iDats0

-- | Split up a 'Df' stream of a vector into multiple independent 'Df' streams.
unbundleVec ::
  forall n dom a .
  (C.KnownNat n, C.NFDataX a, C.HiddenClockResetEnable dom, 1 <= n) =>
  Circuit (Df dom (C.Vec n a)) (C.Vec n (Df dom a))
unbundleVec
  = Circuit (B.second C.unbundle . C.mealyB go initState . B.second C.bundle)
 where
  initState :: C.Vec n Bool
  initState = C.repeat False

  go _ (NoData, _) = (initState, (Ack False, C.repeat NoData))
  go acked (Data payloadVec, acks) =
    let
      -- Send data to "clients" that have not acked yet
      valids_ = C.map not acked
      dats0 = C.zipWith (\d -> bool Nothing (Just d)) payloadVec valids_
      dats1 = C.map maybeToData dats0

      -- Store new acks, send ack if all "clients" have acked
      acked1 = C.zipWith (||) acked (C.map (\(Ack a) -> a) acks)
      ack = C.fold @(n C.- 1) (&&) acked1
    in
      ( if ack then initState else acked1
      , (Ack ack, dats1) )

-- | Distribute data across multiple components on the RHS. Useful if you want
-- to parallelize a workload across multiple (slow) workers. For optimal
-- throughput, you should make sure workers can accept data every /n/ cycles.
roundrobin ::
  forall n dom a .
  (C.KnownNat n, C.HiddenClockResetEnable dom, 1 <= n) =>
  Circuit (Df dom a) (C.Vec n (Df dom a))
roundrobin
  = Circuit
  ( B.second C.unbundle
  . C.mealyB go (minBound :: C.Index n)
  . B.second C.bundle)
 where
  go i0 (NoData, _) = (i0, (Ack False, C.repeat NoData))
  go i0 (Data dat, acks) =
    let
      datOut0 = C.replace i0 (Just dat) (C.repeat Nothing)
      datOut1 = C.map maybeToData datOut0
      i1 = if ack then C.satSucc C.SatWrap i0 else i0
      Ack ack = acks C.!! i0
    in
      (i1, (Ack ack, datOut1))

-- | Collect mode in 'roundrobinCollect'
data CollectMode
  -- | Collect in a /roundrobin/ fashion. If a component does not produce
  -- data, wait until it does.
  = NoSkip
  -- | Collect in a /roundrobin/ fashion. If a component does not produce
  -- data, skip it and check the next component on the next cycle.
  | Skip
  -- | Check all components in parallel. Biased towards the /last/ Df
  -- channel.
  | Parallel

-- | Opposite of 'roundrobin'. Useful to collect data from workers that only
-- produce a result with an interval of /n/ cycles.
roundrobinCollect ::
  forall n dom a .
  (C.KnownNat n, C.HiddenClockResetEnable dom, 1 <= n) =>
  CollectMode ->
  Circuit (C.Vec n (Df dom a)) (Df dom a)
roundrobinCollect NoSkip
  = Circuit (B.first C.unbundle . C.mealyB go minBound . B.first C.bundle)
 where
  go (i :: C.Index n) (dats, Ack ack) =
    case (dats C.!! i) of
      Data d ->
        ( if ack then C.satSucc C.SatWrap i else i
        , ( C.replace i (Ack ack) (C.repeat (Ack False))
          , Data d ))
      NoData ->
        (i, (C.repeat (Ack False), NoData))

roundrobinCollect Skip
  = Circuit (B.first C.unbundle . C.mealyB go minBound . B.first C.bundle)
 where
  go (i :: C.Index n) (dats, Ack ack) =
    case (dats C.!! i) of
      Data d ->
        ( if ack then C.satSucc C.SatWrap i else i
        , ( C.replace i (Ack ack) (C.repeat (Ack False))
          , Data d ))
      NoData ->
        (C.satSucc C.SatWrap i, (C.repeat (Ack False), NoData))

roundrobinCollect Parallel
  = Circuit (B.first C.unbundle . C.unbundle . fmap go . C.bundle . B.first C.bundle)
 where
  go (dats0, ack) = (acks, dat1)
   where
    nacks = C.repeat (Ack False)
    acks = Maybe.fromMaybe nacks ((\i -> C.replace i ack nacks) <$> iM)
    dat1 = Maybe.fromMaybe NoData dat0
    (iM, dat0) = Data.List.NonEmpty.unzip dats1
    dats1 = C.fold @(n C.- 1) (<|>) (C.zipWith goDat C.indicesI dats0)

    goDat i dat
      | Maybe.isJust (dataToMaybe dat) = Just (i, dat)
      | otherwise = Nothing

-- | Place register on /forward/ part of a circuit.
registerFwd ::
  forall dom a .
  (C.NFDataX a, C.HiddenClockResetEnable dom) =>
  Circuit (Df dom a) (Df dom a)
registerFwd
  = forceResetSanity |> Circuit (C.mealyB go NoData)
 where
   go s0 (iDat, Ack iAck) = (s1, (Ack oAck, s0))
    where
     oAck = not (Maybe.isJust (dataToMaybe s0)) || iAck
     s1 = if oAck then iDat else s0

-- | Place register on /backward/ part of a circuit. This is implemented using a
-- in-logic two-element shift register.
registerBwd ::
  (C.NFDataX a, C.HiddenClockResetEnable dom) =>
  Circuit (Df dom a) (Df dom a)
registerBwd
  = forceResetSanity |> Circuit (C.mealyB go (NoData, NoData))
 where
   go (ra0, rb) (iDat, Ack iAck) =
     (s, (Ack oAck, rb))
    where
     oAck = Maybe.isNothing (dataToMaybe ra0)
     ra1 = if oAck then iDat else ra0
     s = if Maybe.isNothing (dataToMaybe rb) || iAck then (NoData, ra1) else (ra1, rb)

-- | A fifo buffer with user-provided depth.
-- Uses blockram to store data
fifo ::
  forall dom a depth.
  (C.HiddenClockResetEnable dom, C.KnownNat depth, C.NFDataX a) =>
  C.SNat depth ->
  Circuit (Df dom a) (Df dom a)
fifo fifoDepth = Circuit $ C.hideReset circuitFunction where

  -- implemented using a fixed-size array
  --   write location and read location are both stored
  --   to write, write to current location and move one to the right
  --   to read, read from current location and move one to the right
  --   loop around from the end to the beginning if necessary

  circuitFunction reset (inpA, inpB) = (otpA, otpB) where
    -- initialize bram
    errMsg = "fifo: undefined initial fifo buffer value"
    brRead = C.readNew
             (C.blockRam (C.replicate fifoDepth $ C.errorX errMsg))
             brReadAddr brWrite
    -- run the state machine (a mealy machine)
    (brReadAddr, brWrite, otpA, otpB)
      = C.unbundle
      $ C.mealy machineAsFunction s0
      $ C.bundle (brRead, C.unsafeToHighPolarity reset, inpA, inpB)

  -- when reset is on, set state to initial state and output blank outputs
  machineAsFunction _ (_, True, _, _) = (s0, (0, Nothing, Ack False, NoData))
  machineAsFunction (rAddr,wAddr,amtLeft) (brRead, False, pushData, Ack popped) =
    let -- potentially push an item onto blockram
        maybePush = if amtLeft > 0 then dataToMaybe pushData else Nothing
        brWrite = (wAddr,) <$> maybePush
        -- adjust write address and amount left (output state machine doesn't see amountLeft')
        (wAddr', amtLeft') = if (Maybe.isJust maybePush)
                             then (incIdxLooping wAddr, amtLeft-1)
                             else (wAddr, amtLeft)
        -- if we're about to push onto an empty queue, we can pop immediately instead
        (brRead_, amtLeft_) = if (amtLeft == maxBound && Maybe.isJust maybePush)
                              then (Maybe.fromJust maybePush, amtLeft')
                              else (brRead, amtLeft)
        -- adjust blockram read address and amount left
        (rAddr', amtLeft'') = if (amtLeft_ < maxBound && popped)
                              then (incIdxLooping rAddr, amtLeft'+1)
                              else (rAddr, amtLeft')
        brReadAddr = rAddr'
        -- return our new state and outputs
        otpAck = Maybe.isJust maybePush
        otpDat = if amtLeft_ < maxBound then Data brRead_ else NoData
    in  ((rAddr', wAddr', amtLeft''), (brReadAddr, brWrite, Ack otpAck, otpDat))

  -- initial state
  -- (s0 for input port (taken from class), s0 for output port (taken from class), next read address, next write address, space left in bram)
  s0 = (_0 fifoDepth, _0 fifoDepth, _maxBound fifoDepth)

  -- type level hack
  -- make sure we have the right Index number
  _0 :: (C.KnownNat n) => C.SNat n -> C.Index n
  _0 = P.const 0

  -- type level hack
  -- make sure we have the right Index number
  _maxBound :: (C.KnownNat n) => C.SNat n -> C.Index (n C.+ 1)
  _maxBound = P.const maxBound

  -- loop around to 0 if we're about to overflow, otherwise increment
  incIdxLooping idx = if idx == maxBound then 0 else idx+1

--------------------------------- SIMULATE -------------------------------------

-- | Emit values given in list. Emits no data while reset is asserted. Not
-- synthesizable.
drive ::
  forall dom a.
  ( C.KnownDomain dom ) =>
  SimulationConfig ->
  [Maybe a] ->
  Circuit () (Df dom a)
drive SimulationConfig{resetCycles} s0 = Circuit $
    ((),)
  . C.fromList_lazy
  . go s0 resetCycles
  . CE.sample_lazy
  . P.snd
 where
  go _ resetN  ~(ack:acks) | resetN > 0 =
    NoData : (ack `C.seqX` go s0 (resetN - 1) acks)
  go [] _ ~(ack:acks) =
    NoData : (ack `C.seqX` go [] 0 acks)
  go (Nothing:is)  _ ~(ack:acks) =
    NoData : (ack `C.seqX` go is 0 acks)
  go (Just dat:is)  _ ~(Ack ack:acks) =
    Data dat : go (if ack then is else Just dat:is) 0 acks

-- | Sample protocol to a list of values. Drops values while reset is asserted.
-- Not synthesizable.
--
-- For a generalized version of 'sample', check out 'sampleC'.
sample ::
  forall dom b.
  ( C.KnownDomain dom ) =>
  SimulationConfig ->
  Circuit () (Df dom b) ->
  [Maybe b]
sample SimulationConfig{..} c =
    fmap dataToMaybe
  $ P.take timeoutAfter
  $ CE.sample_lazy
  $ ignoreWhileInReset
  $ P.snd
  $ toSignals c ((), Ack <$> rst_n)
 where
  ignoreWhileInReset s =
    (uncurry (bool NoData)) <$>
    C.bundle (s, rst_n)

  rst_n = C.fromList (replicate resetCycles False <> repeat True)

-- | Stall every valid Df packet with a given number of cycles. If there are
-- more valid packets than given numbers, passthrough all valid packets without
-- stalling. Not synthesizable.
--
-- For a generalized version of 'stall', check out 'stallC'.
stall ::
  forall dom a.
  ( C.KnownDomain dom
  , HasCallStack ) =>
  SimulationConfig ->
  -- | Acknowledgement to send when LHS does not send data. Stall will act
  -- transparently when reset is asserted.
  StallAck ->
  -- Number of cycles to stall for every valid Df packet
  [Int] ->
  Circuit (Df dom a) (Df dom a)
stall SimulationConfig{..} stallAck stalls = Circuit $
  uncurry (go stallAcks stalls resetCycles)
 where
  stallAcks
    | stallAck == StallCycle = [minBound..maxBound] \\ [StallCycle]
    | otherwise = [stallAck]

  toStallAck :: Maybe a -> Ack -> StallAck -> Ack
  toStallAck (Just _) ack = P.const ack
  toStallAck Nothing ack = \case
    StallWithNack -> Ack False
    StallWithAck -> Ack True
    StallWithErrorX -> C.errorX "No defined ack"
    StallTransparently -> ack
    StallCycle -> Ack False -- shouldn't happen..

  go ::
    [StallAck] ->
    [Int] ->
    Int ->
    Signal dom (Data a) ->
    Signal dom Ack ->
    ( Signal dom Ack
    , Signal dom (Data a) )
  go [] ss rs fwd bwd =
    go stallAcks ss rs fwd bwd

  go (_:sas) _ resetN (f :- fwd) ~(b :- bwd) | resetN > 0 =
    B.bimap (b :-) (f :-) (go sas stalls (resetN - 1) fwd bwd)

  go (sa:sas) [] _ (f :- fwd) ~(b :- bwd) =
    B.bimap (toStallAck (dataToMaybe f) b sa :-) (f :-) (go sas [] 0 fwd bwd)

  go (sa:sas) ss _ (NoData :- fwd) ~(b :- bwd) =
    -- Left hand side does not send data, simply replicate that behavior. Right
    -- hand side might send an arbitrary acknowledgement, so we simply pass it
    -- through.
    B.bimap (toStallAck Nothing b sa :-) (NoData :-) (go sas ss 0 fwd bwd)
  go (_sa:sas) (s:ss) _ (f0 :- fwd) ~(Ack b0 :- bwd) =
    let
      -- Stall as long as s > 0. If s ~ 0, we wait for the RHS to acknowledge
      -- the data. As long as RHS does not acknowledge the data, we keep sending
      -- the same data.
      (f1, b1, s1) = case compare 0 s of
        LT -> (NoData, Ack False, pred s:ss)    -- s > 0
        EQ -> (f0, Ack b0, if b0 then ss else s:ss) -- s ~ 0
        GT -> error ("Unexpected negative stall: " <> show s) -- s < 0
    in
      B.bimap (b1 :-) (f1 :-) (go sas s1 0 fwd bwd)

-- | Simulate a single domain protocol. Not synthesizable.
--
-- For a generalized version of 'simulate', check out 'Protocols.simulateC'.
simulate ::
  forall dom a b.
  ( C.KnownDomain dom ) =>
  -- | Simulation configuration. Use 'Data.Default.def' for sensible defaults.
  SimulationConfig ->
  -- | Circuit to simulate.
  ( C.Clock dom ->
    C.Reset dom ->
    C.Enable dom ->
    Circuit (Df dom a) (Df dom b) ) ->
  -- | Inputs
  [Maybe a] ->
  -- | Outputs
  [Maybe b]
simulate conf@SimulationConfig{..} circ inputs =
  sample conf (drive conf inputs |> circ clk rst ena)
 where
  (clk, rst, ena) = (C.clockGen, resetGen resetCycles, C.enableGen)

-- | Like 'C.resetGenN', but works on 'Int' instead of 'C.SNat'. Not
-- synthesizable.
resetGen :: C.KnownDomain dom => Int -> C.Reset dom
resetGen n = C.unsafeFromHighPolarity
  (C.fromList (replicate n True <> repeat False))
