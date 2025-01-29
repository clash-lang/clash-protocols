{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
-- TODO: Fix warnings introduced by GHC 9.2 w.r.t. incomplete lazy pattern matches
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{- |
Defines data structures and operators to create a Dataflow protocol that only
carries data, no metadata. For documentation see:

  * 'Protocols.Circuit'
  * 'Protocols.Df.Df'
-}
module Protocols.Df (
  -- * Types
  Df,
  Data (..),

  -- * Operations on Df protocol
  empty,
  const,
  consume,
  void,
  pure,
  map,
  mapS,
  bimap,
  bimapS,
  fst,
  snd,
  mapMaybe,
  catMaybes,
  coerce,
  compressor,
  expander,
  compander,
  filter,
  filterS,
  either,
  eitherS,
  first {-firstT,-},
  firstS,
  mapLeft,
  mapLeftS,
  second {-secondT,-},
  secondS,
  mapRight,
  mapRightS,
  zipWith,
  zipWithS,
  zip,
  partition,
  partitionS,
  route,
  select,
  selectN,
  selectUntil,
  selectUntilS,
  fanin,
  faninS,
  mfanin,
  fanout,
  bundleVec,
  unbundleVec,
  roundrobin,
  CollectMode (..),
  roundrobinCollect,
  registerFwd,
  registerBwd,
  fifo,

  -- * Simulation functions
  drive,
  stall,
  sample,
  simulate,

  -- * Internals
  forceResetSanity,
  dataToMaybe,
  maybeToData,
  hasData,
  noData,
  fromData,
  toData,
) where

-- base
#if !MIN_VERSION_base(4,18,0)
import           Control.Applicative (Applicative(liftA2))
#endif
import Control.Applicative (Alternative ((<|>)))
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Prelude hiding (
  const,
  either,
  filter,
  fst,
  map,
  pure,
  snd,
  zip,
  zipWith,
  (!!),
 )

import qualified Data.Bifunctor as B
import Data.Bool (bool)
import qualified Data.Coerce as Coerce
#if MIN_VERSION_base(4,19,0)
import qualified Data.Functor as Functor (unzip)
#else
import qualified Data.List.NonEmpty as Functor (unzip)
#endif
import Data.Kind (Type)
import Data.List ((\\))
import qualified Data.Maybe as Maybe
import Data.Proxy
import qualified Prelude as P

-- clash-prelude

import qualified Clash.Explicit.Prelude as CE
import Clash.Prelude (type (<=))
import qualified Clash.Prelude as C
import Clash.Signal.Internal (Signal (..))

-- me
import Protocols.Idle
import Protocols.Internal

{-# ANN module "HLint: ignore Use const" #-}

{- $setup
>>> import Protocols
>>> import Clash.Prelude (Vec(..))
>>> import qualified Prelude as P
>>> import qualified Data.Bifunctor as B
-}

{- | Like 'Protocols.Df', but without metadata.

__N.B.__: For performance reasons 'Protocols.Data' is strict on
its data field. That is, if 'Protocols.Data' is evaluated to WHNF,
its fields will be evaluated to WHNF too.
-}
data Df (dom :: C.Domain) (a :: Type)

instance Protocol (Df dom a) where
  -- \| Forward part of simple dataflow: @Signal dom (Data meta a)@
  type Fwd (Df dom a) = Signal dom (Data a)

  -- \| Backward part of simple dataflow: @Signal dom Bool@
  type Bwd (Df dom a) = Signal dom Ack

instance Backpressure (Df dom a) where
  boolsToBwd _ = C.fromList_lazy . Coerce.coerce

{- | Data sent over forward channel of 'Df'. Note that this data type is strict
on its data field.
-}
data Data a
  = -- | Send no data
    NoData
  | -- | Send /a/
    Data !a
  deriving (Functor, Generic, C.NFDataX, C.ShowX, Eq, NFData, Show, C.Bundle)

instance Applicative Data where
  pure = Data

  liftA2 f (Data a) (Data b) = Data (f a b)
  liftA2 _ _ _ = NoData

instance Alternative Data where
  empty = NoData

  Data a <|> _ = Data a
  _ <|> b = b

instance Monad Data where
  (>>=) :: Data a -> (a -> Data b) -> Data b
  NoData >>= _f = NoData
  Data a >>= f = f a

instance IdleCircuit (Df dom a) where
  idleFwd _ = C.pure NoData
  idleBwd _ = C.pure (Ack False)

-- | Convert 'Data' to 'Maybe'. Produces 'Just' on 'Data', 'Nothing' on 'NoData'.
dataToMaybe :: Data a -> Maybe a
dataToMaybe NoData = Nothing
dataToMaybe (Data a) = Just a

-- | Convert 'Maybe' to 'Data'. Produces 'Data' on 'Just', 'NoData' on 'Nothing'.
maybeToData :: Maybe a -> Data a
maybeToData Nothing = NoData
maybeToData (Just a) = Data a

-- | True if `Data` contains a value.
hasData :: Data a -> Bool
hasData NoData = False
hasData (Data _) = True

-- | True if `Data` contains no value.
noData :: Data a -> Bool
noData NoData = True
noData (Data _) = False

-- | Extract value from `Data`, Bottom on `NoData`.
fromData :: (HasCallStack, C.NFDataX a) => Data a -> a
fromData NoData = C.deepErrorX "fromData: NoData"
fromData (Data a) = a

-- | Construct a `Data` if bool is True, `NoData` otherwise.
toData :: Bool -> a -> Data a
toData False _ = NoData
toData True a = Data a

instance (C.KnownDomain dom, C.NFDataX a, C.ShowX a, Show a) => Simulate (Df dom a) where
  type SimulateFwdType (Df dom a) = [Data a]
  type SimulateBwdType (Df dom a) = [Ack]
  type SimulateChannels (Df dom a) = 1

  simToSigFwd _ = C.fromList_lazy
  simToSigBwd _ = C.fromList_lazy
  sigToSimFwd _ s = C.sample_lazy s
  sigToSimBwd _ s = C.sample_lazy s

  stallC conf (C.head -> (stallAck, stalls)) = stall conf stallAck stalls

instance (C.KnownDomain dom, C.NFDataX a, C.ShowX a, Show a) => Drivable (Df dom a) where
  type ExpectType (Df dom a) = [a]

  toSimulateType Proxy = P.map Data
  fromSimulateType Proxy = Maybe.mapMaybe dataToMaybe

  driveC conf vals = drive conf (dataToMaybe <$> vals)
  sampleC conf ckt = maybeToData <$> sample conf ckt

{- | Force a /nack/ on the backward channel and /no data/ on the forward
channel if reset is asserted.
-}
forceResetSanity ::
  forall dom a.
  (C.HiddenClockResetEnable dom) =>
  Circuit (Df dom a) (Df dom a)
forceResetSanity = forceResetSanityGeneric

-- | Coerce the payload of a Df stream.
coerce :: (Coerce.Coercible a b) => Circuit (Df dom a) (Df dom b)
coerce = fromSignals $ \(fwdA, bwdB) -> (Coerce.coerce bwdB, Coerce.coerce fwdA)

{- | Takes one or more values from the left and "compresses" it into a single
value that is occasionally sent to the right. Useful for taking small high-speed
inputs (like bits from a serial line) and turning them into slower wide outputs
(like 32-bit integers).

Example:

>>> accumulate xs x = let xs' = x:xs in if length xs' == 3 then ([], Just xs') else (xs', Nothing)
>>> circuit = C.exposeClockResetEnable (compressor @C.System [] accumulate)
>>> take 2 (simulateCSE circuit [(1::Int),2,3,4,5,6,7])
[[3,2,1],[6,5,4]]
-}
compressor ::
  forall dom s i o.
  (C.HiddenClockResetEnable dom, C.NFDataX s) =>
  s ->
  -- | Return `Just` when the compressed value is complete.
  (s -> i -> (s, Maybe o)) ->
  Circuit (Df dom i) (Df dom o)
compressor s0 f = compander s0 $
  \s i ->
    let (s', o) = f s i
     in (s', o, True)

{- | Takes a value from the left and "expands" it into one or more values that
are sent off to the right. Useful for taking wide, slow inputs (like a stream of
32-bit integers) and turning them into a fast, narrow output (like a stream of bits).

Example:

>>> step index = if index == maxBound then (0, True) else (index + 1, False)
>>> expandVector index vec = let (index', done) = step index in (index', vec C.!! index, done)
>>> circuit = C.exposeClockResetEnable (expander @C.System (0 :: C.Index 3) expandVector)
>>> take 6 (simulateCSE circuit [1 :> 2 :> 3 :> Nil, 4 :> 5 :> 6 :> Nil])
[1,2,3,4,5,6]
-}
expander ::
  forall dom i o s.
  (C.HiddenClockResetEnable dom, C.NFDataX s) =>
  s ->
  -- | Return `True` when you're finished with the current input value
  -- and are ready for the next one.
  (s -> i -> (s, o, Bool)) ->
  Circuit (Df dom i) (Df dom o)
expander s0 f = compander s0 $
  \s i ->
    let (s', o, done) = f s i
     in (s', Just o, done)

{- | Takes values from the left,
possibly holding them there for a while while working on them,
and occasionally sends values off to the right.
Used to implement both `expander` and `compressor`, so you can use it
when there's not a straightforward one-to-many or many-to-one relationship
between the input and output streams.
-}
compander ::
  forall dom i o s.
  (C.HiddenClockResetEnable dom, C.NFDataX s) =>
  s ->
  -- | Return `True` when you're finished with the current input value
  -- and are ready for the next one.
  -- Return `Just` to send the produced value off to the right.
  (s -> i -> (s, Maybe o, Bool)) ->
  Circuit (Df dom i) (Df dom o)
compander s0 f = forceResetSanity |> Circuit (C.unbundle . go . C.bundle)
 where
  go :: Signal dom (Data i, Ack) -> Signal dom (Ack, Data o)
  go = C.mealy f' s0
  f' :: s -> (Data i, Ack) -> (s, (Ack, Data o))
  f' s (NoData, _) = (s, (Ack False, NoData))
  f' s (Data i, Ack ack) = (s'', (Ack ackBack, maybe NoData Data o))
   where
    (s', o, doneWithInput) = f s i
    -- We only care about the downstream ack if we're sending them something
    mustWaitForAck = Maybe.isJust o
    (s'', ackBack) = if mustWaitForAck && not ack then (s, False) else (s', doneWithInput)

-- | Like 'P.map', but over payload (/a/) of a Df stream.
map :: (a -> b) -> Circuit (Df dom a) (Df dom b)
map f = mapS (C.pure f)

-- | Like 'map', but can reason over signals.
mapS :: Signal dom (a -> b) -> Circuit (Df dom a) (Df dom b)
mapS fS = Circuit (C.unbundle . liftA2 go fS . C.bundle)
 where
  go f (fwd, bwd) = (bwd, f <$> fwd)

-- | Like 'P.map', but over payload (/a/) of a Df stream.
bimap ::
  (B.Bifunctor p) =>
  (a -> b) ->
  (c -> d) ->
  Circuit (Df dom (p a c)) (Df dom (p b d))
bimap f g = bimapS (C.pure f) (C.pure g)

-- | Like 'bimap', but can reason over signals.
bimapS ::
  (B.Bifunctor p) =>
  Signal dom (a -> b) ->
  Signal dom (c -> d) ->
  Circuit (Df dom (p a c)) (Df dom (p b d))
bimapS fS gS = mapS (liftA2 B.bimap fS gS)

-- | Like 'P.fst', but over payload of a Df stream.
fst :: Circuit (Df dom (a, b)) (Df dom a)
fst = map P.fst

-- | Like 'P.snd', but over payload of a Df stream.
snd :: Circuit (Df dom (a, b)) (Df dom b)
snd = map P.snd

-- | Like 'Data.Bifunctor.first', but over payload of a Df stream.
first :: (B.Bifunctor p) => (a -> b) -> Circuit (Df dom (p a c)) (Df dom (p b c))
first f = firstS (C.pure f)

-- | Like 'first', but can reason over signals.
firstS ::
  (B.Bifunctor p) => Signal dom (a -> b) -> Circuit (Df dom (p a c)) (Df dom (p b c))
firstS fS = mapS (B.first <$> fS)

-- | Like 'Data.Bifunctor.second', but over payload of a Df stream.
second :: (B.Bifunctor p) => (b -> c) -> Circuit (Df dom (p a b)) (Df dom (p a c))
second f = secondS (C.pure f)

-- | Like 'second', but can reason over signals.
secondS ::
  (B.Bifunctor p) => Signal dom (b -> c) -> Circuit (Df dom (p a b)) (Df dom (p a c))
secondS fS = mapS (B.second <$> fS)

-- | Acknowledge but ignore data from LHS protocol. Send a static value /b/.
const :: (C.HiddenReset dom) => b -> Circuit (Df dom a) (Df dom b)
const b =
  Circuit
    ( P.const
        ( Ack
            <$> C.unsafeToActiveLow C.hasReset
        , P.pure (Data b)
        )
    )

-- | Never produce a value.
empty :: Circuit () (Df dom a)
empty = Circuit (P.const ((), P.pure NoData))

-- | Drive a constant value composed of /a/.
pure :: a -> Circuit () (Df dom a)
pure a = Circuit (P.const ((), P.pure (Data a)))

-- | Always acknowledge and ignore values.
consume :: (C.HiddenReset dom) => Circuit (Df dom a) ()
consume = Circuit (P.const (P.pure (Ack True), ()))

-- | Never acknowledge values.
void :: (C.HiddenReset dom) => Circuit (Df dom a) ()
void =
  Circuit
    ( P.const
        ( Ack
            <$> C.unsafeToActiveLow C.hasReset
        , ()
        )
    )

{- | Like 'Data.Maybe.catMaybes', but over a Df stream.

Example:

>>> take 2 (simulateCS (catMaybes @C.System @Int) [Nothing, Just 1, Nothing, Just 3])
[1,3]
-}
catMaybes :: Circuit (Df dom (Maybe a)) (Df dom a)
catMaybes = Circuit (C.unbundle . fmap go . C.bundle)
 where
  go (NoData, _) = (Ack False, NoData)
  go (Data Nothing, _) = (Ack True, NoData)
  go (Data (Just a), ack) = (ack, Data a)

-- | Like 'Data.Maybe.mapMaybe', but over payload (/a/) of a Df stream.
mapMaybe :: (a -> Maybe b) -> Circuit (Df dom a) (Df dom b)
mapMaybe f = map f |> catMaybes

{- | Like 'P.filter', but over a 'Df' stream.

Example:

>>> take 3 (simulateCS (filter @C.System @Int (>5)) [1, 5, 7, 10, 3, 11])
[7,10,11]
-}
filter :: forall dom a. (a -> Bool) -> Circuit (Df dom a) (Df dom a)
filter f = filterS (C.pure f)

-- | Like `filter`, but can reason over signals.
filterS :: forall dom a. Signal dom (a -> Bool) -> Circuit (Df dom a) (Df dom a)
filterS fS = Circuit (C.unbundle . liftA2 go fS . C.bundle)
 where
  go _ (NoData, _) = (Ack False, NoData)
  go f (Data d, ack)
    | f d = (ack, Data d)
    | otherwise = (Ack True, NoData)

-- | Like 'Data.Either.Combinators.mapLeft', but over payload of a 'Df' stream.
mapLeft :: (a -> b) -> Circuit (Df dom (Either a c)) (Df dom (Either b c))
mapLeft f = mapLeftS (C.pure f)

-- | Like 'mapLeft', but can reason over signals.
mapLeftS :: Signal dom (a -> b) -> Circuit (Df dom (Either a c)) (Df dom (Either b c))
mapLeftS = firstS

-- | Like 'Data.Either.Combinators.mapRight', but over payload of a 'Df' stream.
mapRight :: (b -> c) -> Circuit (Df dom (Either a b)) (Df dom (Either a c))
mapRight = second

-- | Like 'mapRight', but can reason over signals.
mapRightS :: Signal dom (b -> c) -> Circuit (Df dom (Either a b)) (Df dom (Either a c))
mapRightS = secondS

-- | Like 'Data.Either.either', but over a 'Df' stream.
either :: (a -> c) -> (b -> c) -> Circuit (Df dom (Either a b)) (Df dom c)
either f g = eitherS (C.pure f) (C.pure g)

-- | Like 'either', but can reason over signals.
eitherS ::
  Signal dom (a -> c) -> Signal dom (b -> c) -> Circuit (Df dom (Either a b)) (Df dom c)
eitherS fS gS = mapS (liftA2 P.either fS gS)

{- | Like 'P.zipWith', but over two 'Df' streams.

Example:

>>> take 3 (simulateCS (zipWith @C.System @Int (+)) ([1, 3, 5], [2, 4, 7]))
[3,7,12]
-}
zipWith ::
  forall dom a b c.
  (a -> b -> c) ->
  Circuit
    (Df dom a, Df dom b)
    (Df dom c)
zipWith f = zipWithS (C.pure f)

-- | Like 'zipWith', but can reason over signals.
zipWithS ::
  forall dom a b c.
  Signal dom (a -> b -> c) ->
  Circuit
    (Df dom a, Df dom b)
    (Df dom c)
zipWithS fS =
  Circuit (B.first C.unbundle . C.unbundle . liftA2 go fS . C.bundle . B.first C.bundle)
 where
  go f ((Data a, Data b), ack) = ((ack, ack), Data (f a b))
  go _ _ = ((Ack False, Ack False), NoData)

-- | Like 'P.zip', but over two 'Df' streams.
zip :: forall a b dom. Circuit (Df dom a, Df dom b) (Df dom (a, b))
zip = zipWith (,)

{- | Like 'P.partition', but over 'Df' streams

Example:

>>> let input = [1, 3, 5, 7, 9, 2, 11]
>>> let output = simulateCS (partition @C.System @Int (>5)) input
>>> B.bimap (take 3) (take 4) output
([7,9,11],[1,3,5,2])
-}
partition :: forall dom a. (a -> Bool) -> Circuit (Df dom a) (Df dom a, Df dom a)
partition f = partitionS (C.pure f)

-- | Like `partition`, but can reason over signals.
partitionS ::
  forall dom a. Signal dom (a -> Bool) -> Circuit (Df dom a) (Df dom a, Df dom a)
partitionS fS =
  Circuit (B.second C.unbundle . C.unbundle . liftA2 go fS . C.bundle . B.second C.bundle)
 where
  go f (Data a, (ackT, ackF))
    | f a = (ackT, (Data a, NoData))
    | otherwise = (ackF, (NoData, Data a))
  go _ _ = (Ack False, (NoData, NoData))

{- | Route a 'Df' stream to another corresponding to the index

Example:

>>> let input = [(0, 3), (0, 5), (1, 7), (2, 13), (1, 11), (2, 1)]
>>> let output = simulateCS (route @3 @C.System @Int) input
>>> fmap (take 2) output
[3,5] :> [7,11] :> [13,1] :> Nil
-}
route ::
  forall n dom a.
  (C.KnownNat n) =>
  Circuit (Df dom (C.Index n, a)) (C.Vec n (Df dom a))
route =
  Circuit (B.second C.unbundle . C.unbundle . fmap go . C.bundle . B.second C.bundle)
 where
  -- go :: (Data (C.Index n, a), C.Vec n (Ack a)) -> (Ack (C.Index n, a), C.Vec n (Data a))
  go (Data (i, a), acks) =
    ( acks C.!! i
    , C.replace i (Data a) (C.repeat NoData)
    )
  go _ =
    (Ack False, C.repeat NoData)

{- | Select data from the channel indicated by the 'Df' stream carrying
@Index n@.

Example:

>>> let indices = [1, 1, 2, 0, 2]
>>> let dats = [8] :> [5, 7] :> [9, 1] :> Nil
>>> let output = simulateCS (select @3 @C.System @Int) (dats, indices)
>>> take 5 output
[5,7,9,8,1]
-}
select ::
  forall n dom a.
  (C.KnownNat n) =>
  Circuit (C.Vec n (Df dom a), Df dom (C.Index n)) (Df dom a)
select = selectUntil (P.const True)

{- | Select /selectN/ samples from channel /n/.

Example:

>>> let indices = [(0, 2), (1, 3), (0, 2)]
>>> let dats = [10, 20, 30, 40] :> [11, 22, 33] :> Nil
>>> let circuit = C.exposeClockResetEnable (selectN @2 @10 @C.System @Int)
>>> take 7 (simulateCSE circuit (dats, indices))
[10,20,11,22,33,30,40]
-}
selectN ::
  forall n selectN dom a.
  ( C.HiddenClockResetEnable dom
  , C.KnownNat selectN
  , C.KnownNat n
  ) =>
  Circuit
    (C.Vec n (Df dom a), Df dom (C.Index n, C.Index selectN))
    (Df dom a)
selectN =
  Circuit
    ( B.first (B.first C.unbundle . C.unbundle)
        . C.mealyB go (0 :: C.Index (selectN C.+ 1))
        . B.first (C.bundle . B.first C.bundle)
    )
 where
  go c0 ((dats, datI), Ack iAck)
    -- Select zero samples: don't send any data to RHS, acknowledge index stream
    -- but no data stream.
    | Data (_, 0) <- datI =
        (c0, ((nacks, Ack True), NoData))
    -- Acknowledge data if RHS acknowledges ours. Acknowledge index stream if
    -- we're done.
    | Data (streamI, nSelect) <- datI
    , let dat = dats C.!! streamI
    , Data d <- dat =
        let
          c1 = if iAck then succ c0 else c0
          oAckIndex = c1 == C.extend nSelect
          c2 = if oAckIndex then 0 else c1
          datAcks = C.replace streamI (Ack iAck) nacks
         in
          ( c2
          ,
            ( (datAcks, Ack oAckIndex)
            , Data d
            )
          )
    -- No index from LHS, nothing to do
    | otherwise =
        (c0, ((nacks, Ack False), NoData))
   where
    nacks = C.repeat (Ack False)

{- | Selects samples from channel /n/ until the predicate holds. The cycle in
which the predicate turns true is included.

Example:

>>> let indices = [0, 0, 1, 2]
>>> let channel1 = [(10, False), (20, False), (30, True), (40, True)]
>>> let channel2 = [(11, False), (21, True)]
>>> let channel3 = [(12, False), (22, False), (32, False), (42, True)]
>>> let dats = channel1 :> channel2 :> channel3 :> Nil
>>> take 10 (simulateCS (selectUntil @3 @C.System @(Int, Bool) P.snd) (dats, indices))
[(10,False),(20,False),(30,True),(40,True),(11,False),(21,True),(12,False),(22,False),(32,False),(42,True)]
-}
selectUntil ::
  forall n dom a.
  (C.KnownNat n) =>
  (a -> Bool) ->
  Circuit
    (C.Vec n (Df dom a), Df dom (C.Index n))
    (Df dom a)
selectUntil f = selectUntilS (C.pure f)

-- | Like 'selectUntil', but can reason over signals.
selectUntilS ::
  forall n dom a.
  (C.KnownNat n) =>
  Signal dom (a -> Bool) ->
  Circuit
    (C.Vec n (Df dom a), Df dom (C.Index n))
    (Df dom a)
selectUntilS fS =
  Circuit
    ( B.first (B.first C.unbundle . C.unbundle)
        . C.unbundle
        . liftA2 go fS
        . C.bundle
        . B.first (C.bundle . B.first C.bundle)
    )
 where
  nacks = C.repeat (Ack False)

  go f ((dats, dat), Ack ack)
    | Data i <- dat
    , Data d <- dats C.!! i =
        (
          ( C.replace i (Ack ack) nacks
          , Ack (f d && ack)
          )
        , Data d
        )
    | otherwise =
        ((nacks, Ack False), NoData)

{- | Copy data of a single 'Df' stream to multiple. LHS will only receive
an acknowledgement when all RHS receivers have acknowledged data.
-}
fanout ::
  forall n dom a.
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
          , (Ack ack, dats)
          )

-- | Merge data of multiple 'Df' streams using a user supplied function
fanin ::
  forall n dom a.
  (C.KnownNat n, 1 <= n) =>
  (a -> a -> a) ->
  Circuit (C.Vec n (Df dom a)) (Df dom a)
fanin f = faninS (C.pure f)

-- | Like 'fanin', but can reason over signals.
faninS ::
  forall n dom a.
  (C.KnownNat n, 1 <= n) =>
  Signal dom (a -> a -> a) ->
  Circuit (C.Vec n (Df dom a)) (Df dom a)
faninS fS = bundleVec |> mapS (C.fold @(n C.- 1) <$> fS)

-- | Merge data of multiple 'Df' streams using Monoid's '<>'.
mfanin ::
  forall n dom a.
  (C.KnownNat n, Monoid a, 1 <= n) =>
  Circuit (C.Vec n (Df dom a)) (Df dom a)
mfanin = fanin (<>)

-- | Bundle a vector of 'Df' streams into one.
bundleVec ::
  forall n dom a.
  (C.KnownNat n, 1 <= n) =>
  Circuit (C.Vec n (Df dom a)) (Df dom (C.Vec n a))
bundleVec =
  Circuit (B.first C.unbundle . C.unbundle . fmap go . C.bundle . B.first C.bundle)
 where
  go (iDats0, iAck) = (C.repeat oAck, maybeToData dat)
   where
    oAck = maybe (Ack False) (P.const iAck) dat
    dat = traverse dataToMaybe iDats0

-- | Split up a 'Df' stream of a vector into multiple independent 'Df' streams.
unbundleVec ::
  forall n dom a.
  (C.KnownNat n, C.NFDataX a, C.HiddenClockResetEnable dom, 1 <= n) =>
  Circuit (Df dom (C.Vec n a)) (C.Vec n (Df dom a))
unbundleVec =
  Circuit (B.second C.unbundle . C.mealyB go initState . B.second C.bundle)
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
      , (Ack ack, dats1)
      )

{- | Distribute data across multiple components on the RHS. Useful if you want
to parallelize a workload across multiple (slow) workers. For optimal
throughput, you should make sure workers can accept data every /n/ cycles.
-}
roundrobin ::
  forall n dom a.
  (C.KnownNat n, C.HiddenClockResetEnable dom, 1 <= n) =>
  Circuit (Df dom a) (C.Vec n (Df dom a))
roundrobin =
  Circuit
    ( B.second C.unbundle
        . C.mealyB go (minBound :: C.Index n)
        . B.second C.bundle
    )
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

-- | Collect modes for dataflow arbiters.
data CollectMode
  = -- | Collect in a /round-robin/ fashion. If a source does not produce
    -- data, wait until it does. Use with care, as there is a risk of
    -- starvation if a selected source is idle for a long time.
    NoSkip
  | -- | Collect in a /round-robin/ fashion. If a source does not produce
    -- data, skip it and check the next source on the next cycle.
    Skip
  | -- | Check all sources in parallel. Biased towards the /last/ source.
    -- If the number of sources is high, this is more expensive than other
    -- modes.
    Parallel

{- | Opposite of 'roundrobin'. Useful to collect data from workers that only
produce a result with an interval of /n/ cycles.
-}
roundrobinCollect ::
  forall n dom a.
  (C.KnownNat n, C.HiddenClockResetEnable dom, 1 <= n) =>
  CollectMode ->
  Circuit (C.Vec n (Df dom a)) (Df dom a)
roundrobinCollect NoSkip =
  Circuit (B.first C.unbundle . C.mealyB go minBound . B.first C.bundle)
 where
  go (i :: C.Index n) (dats, Ack ack) =
    case (dats C.!! i) of
      Data d ->
        ( if ack then C.satSucc C.SatWrap i else i
        ,
          ( C.replace i (Ack ack) (C.repeat (Ack False))
          , Data d
          )
        )
      NoData ->
        (i, (C.repeat (Ack False), NoData))
roundrobinCollect Skip =
  Circuit (B.first C.unbundle . C.mealyB go minBound . B.first C.bundle)
 where
  go (i :: C.Index n) (dats, Ack ack) =
    case (dats C.!! i) of
      Data d ->
        ( if ack then C.satSucc C.SatWrap i else i
        ,
          ( C.replace i (Ack ack) (C.repeat (Ack False))
          , Data d
          )
        )
      NoData ->
        (C.satSucc C.SatWrap i, (C.repeat (Ack False), NoData))
roundrobinCollect Parallel =
  Circuit (B.first C.unbundle . C.unbundle . fmap go . C.bundle . B.first C.bundle)
 where
  go (dats0, ack) = (acks, dat1)
   where
    nacks = C.repeat (Ack False)
    acks = Maybe.fromMaybe nacks ((\i -> C.replace i ack nacks) <$> iM)
    dat1 = Maybe.fromMaybe NoData dat0
    (iM, dat0) = Functor.unzip dats1
    dats1 = C.fold @(n C.- 1) (<|>) (C.zipWith goDat C.indicesI dats0)

    goDat i dat
      | Maybe.isJust (dataToMaybe dat) = Just (i, dat)
      | otherwise = Nothing

-- | Place register on /forward/ part of a circuit. This adds combinational delay on the /backward/ path.
registerFwd ::
  forall dom a.
  (C.NFDataX a, C.HiddenClockResetEnable dom) =>
  Circuit (Df dom a) (Df dom a)
registerFwd =
  forceResetSanity |> Circuit (C.mealyB go NoData)
 where
  go s0 (iDat, Ack iAck) = (s1, (Ack oAck, s0))
   where
    oAck = Maybe.isNothing (dataToMaybe s0) || iAck
    s1 = if oAck then iDat else s0

-- | Place register on /backward/ part of a circuit. This adds combinational delay on the /forward/ path.
registerBwd ::
  forall dom a.
  (C.NFDataX a, C.HiddenClockResetEnable dom) =>
  Circuit (Df dom a) (Df dom a)
registerBwd =
  forceResetSanity |> Circuit go
 where
  go (iDat, iAck) = (Ack <$> oAck, oDat)
   where
    oAck = C.regEn True valid (Coerce.coerce <$> iAck)
    valid = (hasData <$> iDat) C..||. (fmap not oAck)
    iDatX0 = fromData <$> iDat
    iDatX1 = C.regEn (C.errorX "registerBwd") oAck iDatX0
    oDat = toData <$> valid <*> (C.mux oAck iDatX0 iDatX1)

-- Fourmolu only allows CPP conditions on complete top-level definitions. This
-- function is not exported.
blockRamUNoClear ::
  forall n dom a addr.
  ( HasCallStack
  , C.HiddenClockResetEnable dom
  , C.NFDataX a
  , Enum addr
  , C.NFDataX addr
  , 1 <= n
  ) =>
  C.SNat n ->
  Signal dom addr ->
  Signal dom (Maybe (addr, a)) ->
  Signal dom a
#if MIN_VERSION_clash_prelude(1,9,0)
blockRamUNoClear = C.blockRamU C.NoClearOnReset
#else
blockRamUNoClear n =
  C.blockRamU C.NoClearOnReset n (C.errorX "No reset function")
#endif

{- | A fifo buffer with user-provided depth. Uses blockram to store data. Can
handle simultaneous write and read (full throughput rate).
-}
fifo ::
  forall dom a depth.
  (C.HiddenClockResetEnable dom, C.KnownNat depth, C.NFDataX a, 1 C.<= depth) =>
  C.SNat depth ->
  Circuit (Df dom a) (Df dom a)
fifo fifoDepth = Circuit $ C.hideReset circuitFunction
 where
  -- implemented using a fixed-size array
  --   write location and read location are both stored
  --   to write, write to current location and move one to the right
  --   to read, read from current location and move one to the right
  --   loop around from the end to the beginning if necessary

  circuitFunction reset (inpA, inpB) = (otpA, otpB)
   where
    -- initialize bram
    brRead =
      C.readNew
        (blockRamUNoClear fifoDepth)
        brReadAddr
        brWrite
    -- run the state machine (a mealy machine)
    (brReadAddr, brWrite, otpA, otpB) =
      C.unbundle $
        C.mealy machineAsFunction s0 $
          C.bundle
            ( brRead
            , C.unsafeToActiveHigh reset
            , inpA
            , inpB
            )

  -- when reset is on, set state to initial state and output blank outputs
  machineAsFunction _ (_, True, _, _) = (s0, (0, Nothing, Ack False, NoData))
  machineAsFunction (rAddr0, wAddr0, amtLeft0) (brRead0, False, pushData, Ack popped) =
    let
      -- potentially push an item onto blockram
      maybePush = if amtLeft0 > 0 then dataToMaybe pushData else Nothing
      brWrite = (wAddr0,) <$> maybePush
      -- adjust write address and amount left
      --   (output state machine doesn't see amountLeft')
      (wAddr1, amtLeft1)
        | Just _ <- maybePush = (C.satSucc C.SatWrap wAddr0, amtLeft0 - 1)
        | otherwise = (wAddr0, amtLeft0)
      -- if we're about to push onto an empty queue, we can pop immediately instead
      (brRead1, amtLeft2)
        | Just push <- maybePush, amtLeft0 == maxBound = (push, amtLeft1)
        | otherwise = (brRead0, amtLeft0)
      -- adjust blockram read address and amount left
      (rAddr1, amtLeft3)
        | amtLeft2 < maxBound && popped = (C.satSucc C.SatWrap rAddr0, amtLeft1 + 1)
        | otherwise = (rAddr0, amtLeft1)
      brReadAddr = rAddr1
      -- return our new state and outputs
      otpAck = Maybe.isJust maybePush
      otpDat = if amtLeft2 < maxBound then Data brRead1 else NoData
     in
      ((rAddr1, wAddr1, amtLeft3), (brReadAddr, brWrite, Ack otpAck, otpDat))

  -- initial state
  -- (next read address in bram, next write address in bram, space left in bram)
  -- Addresses only go from 0 to depth-1.
  -- Space left goes from 0 to depth because the fifo could be empty
  -- (space left = depth) or full (space left = 0).
  s0 :: (C.Index depth, C.Index depth, C.Index (depth C.+ 1))
  s0 = (0, 0, maxBound)

--------------------------------- SIMULATE -------------------------------------

{- | Emit values given in list. Emits no data while reset is asserted. Not
synthesizable.
-}
drive ::
  forall dom a.
  (C.KnownDomain dom) =>
  SimulationConfig ->
  [Maybe a] ->
  Circuit () (Df dom a)
drive SimulationConfig{resetCycles} s0 =
  Circuit $
    ((),)
      . C.fromList_lazy
      . go s0 resetCycles
      . CE.sample_lazy
      . P.snd
 where
  go _ resetN ~(ack : acks)
    | resetN > 0 =
        NoData : (ack `C.seqX` go s0 (resetN - 1) acks)
  go [] _ ~(ack : acks) =
    NoData : (ack `C.seqX` go [] 0 acks)
  go (Nothing : is) _ ~(ack : acks) =
    NoData : (ack `C.seqX` go is 0 acks)
  go (Just dat : is) _ ~(Ack ack : acks) =
    Data dat : go (if ack then is else Just dat : is) 0 acks

{- | Sample protocol to a list of values. Drops values while reset is asserted.
Not synthesizable.

For a generalized version of 'sample', check out 'sampleC'.
-}
sample ::
  forall dom b.
  (C.KnownDomain dom) =>
  SimulationConfig ->
  Circuit () (Df dom b) ->
  [Maybe b]
sample SimulationConfig{..} c =
  fmap dataToMaybe $
    P.take timeoutAfter $
      CE.sample_lazy $
        ignoreWhileInReset $
          P.snd $
            toSignals c ((), Ack <$> rst_n)
 where
  ignoreWhileInReset s =
    (uncurry (bool NoData))
      <$> C.bundle (s, rst_n)

  rst_n = C.fromList (replicate resetCycles False <> repeat True)

{- | Stall every valid Df packet with a given number of cycles. If there are
more valid packets than given numbers, passthrough all valid packets without
stalling. Not synthesizable.

For a generalized version of 'stall', check out 'stallC'.
-}
stall ::
  forall dom a.
  ( C.KnownDomain dom
  , HasCallStack
  ) =>
  SimulationConfig ->
  -- | Acknowledgement to send when LHS does not send data. Stall will act
  -- transparently when reset is asserted.
  StallAck ->
  -- Number of cycles to stall for every valid Df packet
  [Int] ->
  Circuit (Df dom a) (Df dom a)
stall SimulationConfig{..} stallAck stalls =
  Circuit $
    uncurry (go stallAcks stalls resetCycles)
 where
  stallAcks
    | stallAck == StallCycle = [minBound .. maxBound] \\ [StallCycle]
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
    , Signal dom (Data a)
    )
  go [] ss rs fwd bwd =
    go stallAcks ss rs fwd bwd
  go (_ : sas) _ resetN (f :- fwd) ~(b :- bwd)
    | resetN > 0 =
        B.bimap (b :-) (f :-) (go sas stalls (resetN - 1) fwd bwd)
  go (sa : sas) [] _ (f :- fwd) ~(b :- bwd) =
    B.bimap (toStallAck (dataToMaybe f) b sa :-) (f :-) (go sas [] 0 fwd bwd)
  go (sa : sas) ss _ (NoData :- fwd) ~(b :- bwd) =
    -- Left hand side does not send data, simply replicate that behavior. Right
    -- hand side might send an arbitrary acknowledgement, so we simply pass it
    -- through.
    B.bimap (toStallAck Nothing b sa :-) (NoData :-) (go sas ss 0 fwd bwd)
  go (_sa : sas) (s : ss) _ (f0 :- fwd) ~(Ack b0 :- bwd) =
    let
      -- Stall as long as s > 0. If s ~ 0, we wait for the RHS to acknowledge
      -- the data. As long as RHS does not acknowledge the data, we keep sending
      -- the same data.
      (f1, b1, s1) = case compare 0 s of
        LT -> (NoData, Ack False, pred s : ss) -- s > 0
        EQ -> (f0, Ack b0, if b0 then ss else s : ss) -- s ~ 0
        GT -> error ("Unexpected negative stall: " <> show s) -- s < 0
     in
      B.bimap (b1 :-) (f1 :-) (go sas s1 0 fwd bwd)

{- | Simulate a single domain protocol. Not synthesizable.

For a generalized version of 'simulate', check out 'Protocols.simulateC'.
-}
simulate ::
  forall dom a b.
  (C.KnownDomain dom) =>
  -- | Simulation configuration. Use 'Data.Default.def' for sensible defaults.
  SimulationConfig ->
  -- | Circuit to simulate.
  ( C.Clock dom ->
    C.Reset dom ->
    C.Enable dom ->
    Circuit (Df dom a) (Df dom b)
  ) ->
  -- | Inputs
  [Maybe a] ->
  -- | Outputs
  [Maybe b]
simulate conf@SimulationConfig{..} circ inputs =
  sample conf (drive conf inputs |> circ clk rst ena)
 where
  (clk, rst, ena) = (C.clockGen, resetGen resetCycles, C.enableGen)

{- | Like 'C.resetGenN', but works on 'Int' instead of 'C.SNat'. Not
synthesizable.
-}
resetGen :: (C.KnownDomain dom) => Int -> C.Reset dom
resetGen n =
  C.unsafeFromActiveHigh
    (C.fromList (replicate n True <> repeat False))
