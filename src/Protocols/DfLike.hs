{-|
Warning: Only read this module if you're considering implementing your own
protocol. Here be dragons.

This module implements a type class 'DfLike' which serves as a generalization
of the 'Protocols.Df.Df' protocol. Similar protocols can provide an instance
for it and subsequently expose all the functionality implemented in this
module. This is set up in such a way that there's little to no overhead compared
to implementing it manually.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Protocols.DfLike
  ( -- * DfLike class
    DfLike(..)

    -- * Operations on Df like protocols
  , const, pure, void
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
  , CollectMode(NoSkip, Skip, Parallel)
  , roundrobinCollect
  , registerFwd
  , registerBwd

    -- * Simulation functions
  , drive
  , stall
  , sample
  , simulate

    -- * Internals
  , forceResetSanity
  ) where

-- base
import qualified Prelude as P
import           Prelude hiding
  (map, const, fst, snd, pure, either, filter, zipWith, zip)
import           Control.Applicative (Alternative ((<|>)))
import           Data.Bool (bool)
import qualified Data.Bifunctor as B
import           Data.Bifunctor (Bifunctor)
import           Data.Kind (Type)
import qualified Data.Maybe as Maybe
import           Data.Proxy (Proxy(..))
import qualified Data.List.NonEmpty
import           GHC.Stack (HasCallStack)

-- me
import           Protocols.Internal hiding (Ack(..))

-- clash-prelude
import           Clash.Prelude (Domain, type (+), type (-), type (<=))
import           Clash.Signal.Internal (Signal(..))
import qualified Clash.Prelude as C
import qualified Clash.Explicit.Prelude as CE
import Data.List ((\\))


-- | Generalization of Df-like protocols (Df, DfMeta, AXI, Avalon, ..). This
-- type class is meant to be a zero-cost abstraction, so it should /not/ be
-- used in a polymorphic context.
--
-- This class is only meant for protocol implementers!
class ( Protocol (df a)
      , Fwd (df a) ~ C.Signal dom (Data df a)
      , Bwd (df a) ~ C.Signal dom (Ack df a)
      ) => DfLike (dom :: Domain) (df :: Type -> Type) (a :: Type) | df a -> dom where
  -- | Data type carried on forward channel
  type Data df a = (r :: Type) | r -> a

  -- | \"Interesting\" data carried on forward channel
  type Payload a = (r :: Type) | r -> a

  -- | Acknowledgement data carried on backward channel
  type Ack df a

  noData ::
    Proxy (df a) ->
    Data df a

  getPayload ::
    Proxy (df a) ->
    Data df a ->
    Maybe (Payload a)

  setPayload ::
    HasCallStack =>
    DfLike dom df b =>
    Proxy (df a) ->
    Proxy (df b) ->
    Data df a ->
    Maybe (Payload b) ->
    Data df b

  boolToAck :: Proxy (df a) -> Bool -> Ack df a
  ackToBool :: Proxy (df a) -> Ack df a -> Bool

hasPayload :: DfLike dom df a => Proxy (df a) -> Data df a -> Bool
hasPayload dfA = Maybe.isJust . getPayload dfA
{-# INLINE hasPayload #-}

mapData ::
  forall df dom a b.
  ( DfLike dom df a
  , DfLike dom df b ) =>
  Proxy (df a) ->
  Proxy (df b) ->
  (Payload a -> Payload b) ->
  Data df a ->
  Data df b
mapData dfA dfB f fwdA =
  setPayload dfA dfB fwdA (fmap f (getPayload dfA fwdA))
{-# INLINE mapData #-}

-- | Like 'P.map'
map ::
  forall df dom a b .
  ( DfLike dom df a
  , DfLike dom df b ) =>
  Proxy (df a) ->
  Proxy (df b) ->
  (Payload a -> Payload b) ->
  Circuit (df a) (df b)
map dfA dfB f = Circuit (uncurry go)
 where
  go fwd bwd =
    ( boolToAck dfA . ackToBool dfB <$> bwd
    , mapData dfA dfB f <$> fwd )
{-# INLINE map #-}

-- | Like 'P.fst'
fst ::
  ( DfLike dom df x
  , DfLike dom df y
  , Payload x ~ (a, b)
  , Payload y ~ a
  ) =>
  Circuit (df x) (df y)
fst = map Proxy Proxy P.fst
{-# INLINE fst #-}

-- | Like 'P.snd'
snd ::
  ( DfLike dom df x
  , DfLike dom df y
  , Payload x ~ (a, b)
  , Payload y ~ b
  ) =>
  Circuit (df x) (df y)
snd = map Proxy Proxy P.snd
{-# INLINE snd #-}

-- | Like 'Data.Bifunctor.bimap'
bimap ::
  ( Bifunctor p
  , DfLike dom df x
  , DfLike dom df y
  , Payload x ~ p a c
  , Payload y ~ p b d
  ) =>
  (a -> b) ->
  (c -> d) ->
  Circuit (df x) (df y)
bimap f g = map Proxy Proxy (B.bimap f g)
{-# INLINE bimap #-}

-- | Like 'Data.Bifunctor.first'
first ::
  ( Bifunctor p
  , DfLike dom df x
  , DfLike dom df y
  , Payload x ~ p a c
  , Payload y ~ p b c
  ) =>
  (a -> b) ->
  Circuit (df x) (df y)
first f = map Proxy Proxy (B.first f)
{-# INLINE first #-}

-- | Like 'Data.Bifunctor.first'
second ::
  ( Bifunctor p
  , DfLike dom df x
  , DfLike dom df y
  , Payload x ~ p a b
  , Payload y ~ p a c
  ) =>
  (b -> c) ->
  Circuit (df x) (df y)
second f = map Proxy Proxy (B.second f)
{-# INLINE second #-}

-- | Acknowledge but ignore data from LHS protocol. Send a static value /b/.
const ::
  ( C.HiddenReset dom
  , DfLike dom df a
  , DfLike dom df b ) =>
  Proxy (df a) ->
  Proxy (df b) ->
  Data df b ->
  Circuit (df a) (df b)
const dfA _dfB b = Circuit
  (P.const (boolToAck dfA <$> C.unsafeToLowPolarity C.hasReset, P.pure b))
{-# INLINE const #-}

-- | Drive a constant value composed of /a/.
pure ::
  DfLike dom df a =>
  Proxy (df a) ->
  Data df a ->
  Circuit () (df a)
pure _dfA a = Circuit (P.const ((), P.pure a))
{-# INLINE pure #-}

-- | Ignore incoming data
void ::
  (C.HiddenReset dom, DfLike dom df a) =>
  Proxy (df a) ->
  Circuit (df a) ()
void dfA = Circuit
  (P.const (boolToAck dfA <$> C.unsafeToLowPolarity C.hasReset, ()))
{-# INLINE void #-}

-- | Like 'Data.Maybe.catMaybes'
catMaybes ::
  forall dom df x y a.
  ( DfLike dom df x
  , DfLike dom df y
  , Payload x ~ Maybe a
  , Payload y ~ a
  ) =>
  Proxy (df x) ->
  Proxy (df y) ->
  Circuit (df x) (df y)
catMaybes dfMa dfA = Circuit (C.unbundle . fmap go . C.bundle)
 where
  go (dat, ack) =
    case getPayload dfMa dat of
      Nothing -> (boolToAck dfMa False, noData dfA)
      Just Nothing -> (boolToAck dfMa True, noData dfA)
      Just (Just a) ->
        ( boolToAck dfMa (ackToBool dfA ack)
        , setPayload dfMa dfA dat (Just a) )
{-# INLINE catMaybes #-}

-- | Like 'Data.Maybe.mapMaybe'
mapMaybe ::
  forall dom df x y a b i .
  ( DfLike dom df x
  , DfLike dom df y
  , DfLike dom df i
  , Payload x ~ a
  , Payload y ~ b
  , Payload i ~ Maybe b
  ) =>
  (a -> Maybe b) ->
  Circuit (df x) (df y)
mapMaybe f = map Proxy Proxy f |> catMaybes Proxy Proxy
{-# INLINE mapMaybe #-}

-- | Like 'P.filter'
filter ::
  forall dom df a.
  DfLike dom df a =>
  Proxy (df a) ->
  (Payload a -> Bool) ->
   Circuit (df a) (df a)
filter dfA f = Circuit (C.unbundle . fmap go . C.bundle)
 where
  go (dat, ack) =
    case getPayload dfA dat of
      Nothing -> (boolToAck dfA False, noData dfA)
      Just d
        | f d ->
          ( boolToAck dfA (ackToBool dfA ack)
          , setPayload dfA dfA dat (Just d) )
        | otherwise -> (boolToAck dfA True, noData dfA)
{-# INLINE filter #-}

-- | Like 'Data.Either.Combinators.mapLeft'
mapLeft ::
  ( DfLike dom df x
  , DfLike dom df y
  , Payload x ~ Either a c
  , Payload y ~ Either b c
  ) =>
  (a -> b) ->
  Circuit (df x) (df y)
mapLeft = first
{-# INLINE mapLeft #-}

-- | Like 'Data.Either.Combinators.mapRight'
mapRight ::
  ( DfLike dom df x
  , DfLike dom df y
  , Payload x ~ Either a b
  , Payload y ~ Either a c
  ) =>
  (b -> c) ->
  Circuit (df x) (df y)
mapRight = second
{-# INLINE mapRight #-}

-- | Like 'Data.Either.either'
either ::
  ( DfLike dom df x
  , DfLike dom df y
  , Payload x ~ Either a b
  , Payload y ~ c
  ) =>
  (a -> c) ->
  (b -> c) ->
  Circuit (df x) (df y)
either f g = map Proxy Proxy (P.either f g)
{-# INLINE either #-}

-- | Like 'P.zipWith'. Any data not in /Payload/ is copied from stream A.
zipWith ::
  forall dom df a b c.
  (DfLike dom df a, DfLike dom df b, DfLike dom df c) =>
  Proxy (df a) ->
  Proxy (df b) ->
  Proxy (df c) ->
  (Payload a -> Payload b -> Payload c) ->
  Circuit
    (df a, df b)
    (df c)
zipWith dfA dfB dfC f =
  Circuit (B.first C.unbundle . C.unbundle . fmap go . C.bundle . B.first C.bundle)
 where
  go ((datA, datB), ack) =
    case (getPayload dfA datA, getPayload dfB datB) of
      (Just a, Just b) ->
        ( (boolToAck dfA (ackToBool dfC ack), boolToAck dfB (ackToBool dfC ack))
        , setPayload dfA dfC datA (Just (f a b)) )
      _ ->
        ( (boolToAck dfA False, boolToAck dfB False)
        , noData dfC )
{-# INLINE zipWith #-}

-- | Like 'P.zip'
zip ::
  forall dom df a b x y z.
  ( DfLike dom df x
  , DfLike dom df y
  , DfLike dom df z
  , Payload x ~ a
  , Payload y ~ b
  , Payload z ~ (a, b) ) =>
  Proxy (df x) ->
  Proxy (df y) ->
  Proxy (df z) ->
  Circuit (df x, df y) (df z)
zip dfX dfY dfZ = zipWith dfX dfY dfZ (,)
{-# INLINE zip #-}

-- | Like 'P.partition'
partition ::
  forall dom df a.
  DfLike dom df a =>
  Proxy (df a) ->
  (Payload a -> Bool) ->
  Circuit (df a) (df a, df a)
partition dfA f =
  Circuit (B.second C.unbundle . C.unbundle . fmap go . C.bundle . B.second C.bundle)
 where
  go (dat, (ackT, ackF)) =
    case getPayload dfA dat of
      Just a
        | f a ->       (ackT, (setPayload dfA dfA dat (Just a), noData dfA))
        | otherwise -> (ackF, (noData dfA, setPayload dfA dfA dat (Just a)))
      Nothing ->
        (boolToAck dfA False, (noData dfA, noData dfA))
{-# INLINE partition #-}

-- | Route a DfLike stream to another corresponding to the index
route ::
  forall n dom df x y a.
  ( C.KnownNat n
  , DfLike dom df x
  , DfLike dom df y
  , Payload x ~ (C.Index n, a)
  , Payload y ~ a
  ) =>
  Proxy (df x) ->
  Proxy (df y) ->
  Circuit (df x) (C.Vec n (df y))
route dfX dfY =
  Circuit (B.second C.unbundle . C.unbundle . fmap go . C.bundle . B.second C.bundle)
 where
  -- go :: (Data (C.Index n, a), C.Vec n (Ack a)) -> (Ack (C.Index n, a), C.Vec n (Data a))
  go (dat@(getPayload dfX -> Just (i, a)), acks) =
    ( boolToAck dfX (ackToBool dfY (acks C.!! i))
    , C.replace i (setPayload dfX dfY dat (Just a)) (C.repeat (noData dfY)) )
  go _ =
    (boolToAck dfX False, C.repeat (noData dfY))
{-# INLINE route #-}

-- | Select data from the channel indicated by the DfLike stream carrying
-- @Index n@.
select ::
  forall n dom df a i.
  ( C.KnownNat n
  , DfLike dom df a
  , DfLike dom df i
  , Payload i ~ C.Index n
  ) =>
  Circuit (C.Vec n (df a), df i) (df a)
select = selectUntil Proxy Proxy (P.const True)
{-# INLINE select #-}

-- | Select /selectN/ samples from channel /n/.
selectN ::
  forall n selectN dom df a i.
  ( C.HiddenClockResetEnable dom
  , C.KnownNat selectN
  , C.KnownNat n
  , DfLike dom df a
  , DfLike dom df i
  , Payload i ~ (C.Index n, C.Index selectN)
  ) =>
  Proxy (df a) ->
  Proxy (df i) ->
  Circuit
    (C.Vec n (df a), df i)
    (df a)
selectN dfA dfI = Circuit $
    B.first (B.first C.unbundle . C.unbundle)
  . C.mealyB go (0 :: C.Index (selectN + 1))
  . B.first (C.bundle . B.first C.bundle)
 where
  go c0 ((dats, datI), ackToBool dfA -> iAck)
    -- Select zero samples: don't send any data to RHS, acknowledge index stream
    -- but no data stream.
    | Just (_, 0) <- getPayload dfI datI
    = (c0, ((nacks, boolToAck dfI True), noData dfA))

    -- Acknowledge data if RHS acknowledges ours. Acknowledge index stream if
    -- we're done.
    | Just (streamI, nSelect) <- getPayload dfI datI
    , let dat = dats C.!! streamI
    , Just d <- getPayload dfA dat
    = let
        c1 = if iAck then succ c0 else c0
        oAckIndex = c1 == C.extend nSelect
        c2 = if oAckIndex then 0 else c1
        datAcks = C.replace streamI (boolToAck dfA iAck) nacks
      in
        (c2, ( (datAcks, boolToAck dfI oAckIndex)
             , setPayload dfA dfA dat (Just d) ))

    -- No index from LHS, nothing to do
    | otherwise
    = (c0, ((nacks, boolToAck dfI False), noData dfA))
   where
    nacks = C.repeat (boolToAck dfA False)
{-# INLINE selectN #-}

-- | Selects samples from channel /n/ until the predicate holds. The cycle in
-- which the predicate turns true is included.
selectUntil ::
  forall n dom df a i.
  ( C.KnownNat n
  , DfLike dom df a
  , DfLike dom df i
  , Payload i ~ C.Index n
  ) =>
  Proxy (df a) ->
  Proxy (df i) ->
  (Payload a -> Bool) ->
  Circuit (C.Vec n (df a), df i) (df a)
selectUntil dfA dfI f = Circuit $
    B.first (B.first C.unbundle . C.unbundle) . C.unbundle
  . fmap go
  . C.bundle . B.first (C.bundle . B.first C.bundle)
 where
  nacks = C.repeat (boolToAck dfA False)

  go ((dats, dat), ack)
    | Just i <- getPayload dfI dat
    , Just d <- getPayload dfA (dats C.!! i)
    = ( ( C.replace i ack nacks
        , boolToAck dfI (f d && ackToBool dfA ack) )
      , setPayload dfA dfA (dats C.!! i) (Just d) )
    | otherwise
    = ((nacks, boolToAck dfI False), noData dfA)
{-# INLINE selectUntil #-}

-- | Forces /no data/ on forward channel and a /nack/ on the backward channel
-- as long as the reset is asserted
forceResetSanity ::
  forall dom df a .
  ( C.HiddenReset dom
  , DfLike dom df a ) =>
  Proxy (df a) ->
  Circuit (df a) (df a)
forceResetSanity dfA =
  Circuit (\(fwd, bwd) -> C.unbundle . fmap f . C.bundle $ (rstLow, fwd, bwd))
 where
  f (True,  _,   _  ) = (boolToAck dfA False, noData dfA)
  f (False, fwd, bwd) = (bwd, fwd)
  rstLow = C.unsafeToHighPolarity (C.hasReset @dom)
{-# INLINE forceResetSanity #-}

-- | Copy data of a single DfLike stream to multiple. LHS will only receive
-- an acknowledgement when all RHS receivers have acknowledged data.
fanout ::
  forall n dom df a .
  ( C.KnownNat n
  , C.HiddenClockResetEnable dom
  , 1 <= n
  , DfLike dom df a
  ) =>
  Proxy (df a) ->
  Circuit (df a) (C.Vec n (df a))
fanout dfA = forceResetSanity dfA |> goC
 where
  goC =
    Circuit $ \(s2r, r2s) ->
      B.second C.unbundle (C.mealyB f initState (s2r, C.bundle r2s))

  initState :: C.Vec n Bool
  initState = C.repeat False

  f acked (dat, acks) =
    case getPayload dfA dat of
      Nothing -> (acked, (boolToAck dfA False, C.repeat (noData dfA)))
      Just _ ->
        -- Data on input
        let
          -- Send data to "clients" that have not acked yet
          valids_ = C.map not acked
          dats = C.map (bool (noData dfA) dat) valids_

          -- Store new acks, send ack if all "clients" have acked
          acked1 = C.zipWith (||) acked (C.map (ackToBool dfA) acks)
          ack = C.fold @_ @(n-1) (&&) acked1
        in
          ( if ack then initState else acked1
          , (boolToAck dfA ack, dats) )
{-# INLINE fanout #-}

-- | Merge data of multiple streams using a user supplied function
fanin ::
  forall n dom df a x y .
  ( C.KnownNat n
  , 1 <= n
  , DfLike dom df x
  , DfLike dom df y
  , Payload x ~ a
  , Payload y ~ C.Vec n a -- This constraint will follow from Protocol instances
  ) =>
  (a -> a -> a) ->
  Circuit (C.Vec n (df x)) (df x)
fanin f = bundleVec Proxy Proxy |> map Proxy Proxy (C.fold @_ @(n-1) f)
{-# INLINE fanin #-}

-- | Merge data of multiple streams using Monoid's '<>'.
mfanin ::
  forall n dom df a x y .
  ( C.KnownNat n
  , Semigroup  a
  , 1 <= n
  , DfLike dom df x
  , DfLike dom df y
  , Payload x ~ a
  , Payload y ~ C.Vec n a -- This constraint will follow from Protocol instances
  ) =>
  Circuit (C.Vec n (df x)) (df x)
mfanin = fanin (<>)
{-# INLINE mfanin #-}

-- | Bundle a vector of DfLike streams into one.
bundleVec ::
  forall n dom df a x y .
  ( C.KnownNat n
  , 1 <= n
  , DfLike dom df x
  , DfLike dom df y
  , Payload x ~ a
  , Payload y ~ C.Vec n a
  ) =>
  Proxy (df x) ->
  Proxy (df y) ->
  Circuit (C.Vec n (df x)) (df y)
bundleVec dfA dfVec =
  Circuit (B.first C.unbundle . C.unbundle . fmap go . C.bundle . B.first C.bundle)
 where
  go (iDats0, iAck) = (C.repeat oAck, setPayload dfA dfVec iDat dat)
   where
    oAck =
      bool
        (boolToAck dfA False)
        (boolToAck dfA (ackToBool dfVec iAck))
        (Maybe.isJust dat)
    dat = traverse (getPayload dfA) iDats0
    iDat = C.head @(n-1) iDats0
{-# INLINE bundleVec #-}

-- | Split up a DfLike stream of a vector into multiple independent DfLike streams.
unbundleVec ::
  forall n dom df a x y .
  ( C.KnownNat n
  , C.NFDataX a
  , C.HiddenClockResetEnable dom
  , 1 <= n
  , DfLike dom df x
  , DfLike dom df y
  , Payload x ~ C.Vec n a
  , Payload y ~ a
  ) =>
  Proxy (df x) ->
  Proxy (df y) ->
  Circuit (df x) (C.Vec n (df y))
unbundleVec dfX dfY =
  Circuit (B.second C.unbundle . C.mealyB go initState . B.second C.bundle)
 where
  initState :: C.Vec n Bool
  initState = C.repeat False

  go acked (dat, acks) =
    case getPayload dfX dat of
      Nothing -> (initState, (boolToAck dfX False, C.repeat (noData dfY)))
      Just payloadVec ->
        let
          -- Send data to "clients" that have not acked yet
          valids_ = C.map not acked
          dats0 = C.zipWith (\d -> bool Nothing (Just d)) payloadVec valids_
          dats1 = C.map (setPayload dfX dfY dat) dats0

          -- Store new acks, send ack if all "clients" have acked
          acked1 = C.zipWith (||) acked (C.map (ackToBool dfY) acks)
          ack = C.fold @_ @(n-1) (&&) acked1
        in
          ( if ack then initState else acked1
          , (boolToAck dfX ack, dats1) )
{-# INLINE unbundleVec #-}

-- | Distribute data across multiple components on the RHS. Useful if you want
-- to parallelize a workload across multiple (slow) workers. For optimal
-- throughput, you should make sure workers can accept data every /n/ cycles.
roundrobin ::
  forall n dom df a .
  ( C.KnownNat n
  , C.HiddenClockResetEnable dom
  , 1 <= n
  , DfLike dom df a
  ) =>
  Proxy (df a) ->
  Circuit (df a) (C.Vec n (df a))
roundrobin dfA =
  Circuit (B.second C.unbundle . C.mealyB go minBound . B.second C.bundle)
 where
  go (i0 :: C.Index n) (datIn, acks) =
    case getPayload dfA datIn of
      Nothing ->
        (i0, (boolToAck dfA False, C.repeat (noData dfA)))
      Just dat ->
        let
          datOut0 = C.replace i0 (Just dat) (C.repeat Nothing)
          datOut1 = C.map (setPayload dfA dfA datIn) datOut0
          i1 = if ack then C.satSucc C.SatWrap i0 else i0
          ack = ackToBool dfA (acks C.!! i0)
        in
          (i1, (boolToAck dfA ack, datOut1))
{-# INLINE roundrobin #-}

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
  forall n dom df a .
  ( C.KnownNat n
  , C.HiddenClockResetEnable dom
  , 1 <= n
  , DfLike dom df a
  ) =>
  Proxy (df a) ->
  CollectMode ->
  Circuit (C.Vec n (df a)) (df a)
roundrobinCollect dfA NoSkip =
  Circuit (B.first C.unbundle . C.mealyB go minBound . B.first C.bundle)
 where
  go ::
    C.Index n ->
    (C.Vec n (Data df a), Ack df a) ->
    (C.Index n, (C.Vec n (Ack df a), Data df a))
  go (i :: C.Index n) ((C.!!i) -> iDat, ackToBool dfA -> ack) =
    case getPayload dfA iDat of
      Just d ->
        ( if ack then C.satSucc C.SatWrap i else i
        , ( C.replace i (boolToAck dfA ack) (C.repeat (boolToAck dfA False))
          , setPayload dfA dfA iDat (Just d) ))
      Nothing ->
        (i, (C.repeat (boolToAck dfA False), noData dfA))

roundrobinCollect dfA Skip =
  Circuit (B.first C.unbundle . C.mealyB go minBound . B.first C.bundle)
 where
  go ::
    C.Index n ->
    (C.Vec n (Data df a), Ack df a) ->
    (C.Index n, (C.Vec n (Ack df a), Data df a))
  go i ((C.!!i) -> iDat, ackToBool dfA -> ack) =
    case getPayload dfA iDat of
      Just d ->
        ( if ack then C.satSucc C.SatWrap i else i
        , ( C.replace i (boolToAck dfA ack) (C.repeat (boolToAck dfA False))
          , setPayload dfA dfA iDat (Just d) ))
      Nothing ->
        (C.satSucc C.SatWrap i, (C.repeat (boolToAck dfA False), noData dfA))

roundrobinCollect dfA Parallel =
  Circuit (B.first C.unbundle . C.unbundle . fmap go . C.bundle . B.first C.bundle)
 where
  go ::
    (C.Vec n (Data df a), Ack df a) ->
    (C.Vec n (Ack df a), Data df a)
  go (dats0, ack) = (acks, dat1)
   where
    nacks = C.repeat (boolToAck dfA False)
    acks = Maybe.fromMaybe nacks ((\i -> C.replace i ack nacks) <$> iM)
    dat1 = Maybe.fromMaybe (noData dfA) dat0
    (iM, dat0) = Data.List.NonEmpty.unzip dats1
    dats1 = C.fold @_ @(n-1) (<|>) (C.zipWith goDat C.indicesI dats0)

    goDat i dat
      | hasPayload dfA dat = Just (i, dat)
      | otherwise = Nothing
{-# INLINE roundrobinCollect #-}

-- | Place register on /forward/ part of a circuit.
registerFwd ::
  forall dom df a .
  ( C.NFDataX (Data df a)
  , C.HiddenClockResetEnable dom
  , DfLike dom df a
  ) =>
  Proxy (df a) ->
  Circuit (df a) (df a)
registerFwd dfA = forceResetSanity dfA |> Circuit (C.mealyB go (noData dfA))
 where
   go s0 (iDat, iAck) = (s1, (boolToAck dfA oAck, s0))
    where
     oAck = not (hasPayload dfA s0) || ackToBool dfA iAck
     s1 = if oAck then iDat else s0
{-# INLINE registerFwd #-}

-- | Place register on /backward/ part of a circuit. This is implemented using a
-- in-logic two-element shift register.
registerBwd ::
  ( C.NFDataX (Data df a)
  , C.HiddenClockResetEnable dom
  , DfLike dom df a
  ) =>
  Proxy (df a) ->
  Circuit (df a) (df a)
registerBwd dfA =
  forceResetSanity dfA |> Circuit (C.mealyB go (noData dfA, noData dfA))
 where
  go (ra0, rb) (iDat, ackToBool dfA -> iAck) =
    (s, (boolToAck dfA oAck, rb))
   where
    oAck = not (hasPayload dfA ra0)
    ra1 = if oAck then iDat else ra0
    s = if not (hasPayload dfA rb) || iAck then (noData dfA, ra1) else (ra1, rb)
{-# INLINE registerBwd #-}

-- | Emit values given in list. Emits no data while reset is asserted. Not
-- synthesizable.
drive ::
  forall dom df a.
  ( C.KnownDomain dom
  , DfLike dom df a ) =>
  Proxy (df a) ->
  SimulationConfig ->
  [Data df a] ->
  Circuit () (df a)
drive dfA SimulationConfig{resetCycles} s0 = Circuit $
    ((),)
  . C.fromList_lazy
  . go s0 resetCycles
  . CE.sample_lazy
  . P.snd
 where
  go _ resetN  ~(ack:acks) | resetN > 0 =
    noData dfA : (ack `C.seqX` go s0 (resetN - 1) acks)
  go [] _ ~(ack:acks) =
    noData dfA : (ack `C.seqX` go [] 0 acks)
  go (dat:is)  _ ~(ack:acks)
    | hasPayload dfA dat = dat : go (if ackToBool dfA ack then is else dat:is) 0 acks
    | otherwise = noData dfA : (ack `C.seqX` go is 0 acks)

-- | Sample protocol to a list of values. Drops values while reset is asserted.
-- Not synthesizable.
--
-- For a generalized version of 'sample', check out 'sampleC'.
sample ::
  forall dom df b.
  ( C.KnownDomain dom
  , DfLike dom df b ) =>
  Proxy (df b) ->
  SimulationConfig ->
  Circuit () (df b) ->
  [Data df b]
sample dfB SimulationConfig{..} c =
    P.take timeoutAfter
  $ CE.sample_lazy
  $ ignoreWhileInReset
  $ P.snd
  $ toSignals c ((), boolToAck dfB <$> rst_n)
 where
  ignoreWhileInReset s =
    (uncurry (bool (noData dfB))) <$>
    C.bundle (s, rst_n)

  rst_n = C.fromList (replicate resetCycles False <> repeat True)

-- | Stall every valid Df packet with a given number of cycles. If there are
-- more valid packets than given numbers, passthrough all valid packets without
-- stalling. Not synthesizable.
--
-- For a generalized version of 'stall', check out 'stallC'.
stall ::
  forall dom df a.
  ( C.KnownDomain dom
  , HasCallStack
  , DfLike dom df a ) =>
  Proxy (df a) ->
  SimulationConfig ->
  -- | Acknowledgement to send when LHS does not send data. Stall will act
  -- transparently when reset is asserted.
  StallAck ->
  -- Number of cycles to stall for every valid Df packet
  [Int] ->
  Circuit (df a) (df a)
stall dfA SimulationConfig{..} stallAck stalls = Circuit $
  uncurry (go stallAcks stalls resetCycles)
 where
  stallAcks
    | stallAck == StallCycle = [minBound..maxBound] \\ [StallCycle]
    | otherwise = [stallAck]

  toStallAck :: Maybe (Payload a) -> Ack df a -> StallAck -> Ack df a
  toStallAck (Just _) ack = P.const ack
  toStallAck Nothing ack = \case
    StallWithNack -> boolToAck dfA False
    StallWithAck -> boolToAck dfA True
    StallWithErrorX -> C.errorX "No defined ack"
    StallTransparently -> ack
    StallCycle -> boolToAck dfA False -- shouldn't happen..

  go ::
    [StallAck] ->
    [Int] ->
    Int ->
    Signal dom (Data df a) ->
    Signal dom (Ack df a) ->
    ( Signal dom (Ack df a)
    , Signal dom (Data df a) )
  go [] ss rs fwd bwd =
    go stallAcks ss rs fwd bwd

  go (_:sas) _ resetN (f :- fwd) ~(b :- bwd) | resetN > 0 =
    B.bimap (b :-) (f :-) (go sas stalls (resetN - 1) fwd bwd)

  go (sa:sas) [] _ (f :- fwd) ~(b :- bwd) =
    B.bimap (toStallAck (getPayload dfA f) b sa :-) (f :-) (go sas [] 0 fwd bwd)

  go (sa:sas) ss _ ((getPayload dfA -> Nothing) :- fwd) ~(b :- bwd) =
    -- Left hand side does not send data, simply replicate that behavior. Right
    -- hand side might send an arbitrary acknowledgement, so we simply pass it
    -- through.
    B.bimap (toStallAck Nothing b sa :-) (noData dfA :-) (go sas ss 0 fwd bwd)
  go (_sa:sas) (s:ss) _ (f0 :- fwd) ~(b0 :- bwd) =
    let
      -- Stall as long as s > 0. If s ~ 0, we wait for the RHS to acknowledge
      -- the data. As long as RHS does not acknowledge the data, we keep sending
      -- the same data.
      (f1, b1, s1) = case compare 0 s of
        LT -> (noData dfA, boolToAck dfA False, pred s:ss)    -- s > 0
        EQ -> (f0, b0, if ackToBool dfA b0 then ss else s:ss) -- s ~ 0
        GT -> error ("Unexpected negative stall: " <> show s) -- s < 0
    in
      B.bimap (b1 :-) (f1 :-) (go sas s1 0 fwd bwd)

-- | Simulate a single domain protocol. Not synthesizable.
--
-- For a generalized version of 'simulate', check out 'Protocols.simulateC'.
simulate ::
  forall dom df a b.
  ( C.KnownDomain dom
  , DfLike dom df a
  , DfLike dom df b ) =>
  Proxy (df a) ->
  Proxy (df b) ->
  -- | Simulation configuration. Use 'Data.Default.def' for sensible defaults.
  SimulationConfig ->
  -- | Circuit to simulate.
  ( C.Clock dom ->
    C.Reset dom ->
    C.Enable dom ->
    Circuit (df a) (df b) ) ->
  -- | Inputs
  [Data df a] ->
  -- | Outputs
  [Data df b]
simulate dfA dfB conf@SimulationConfig{..} circ inputs =
  sample dfB conf (drive dfA conf inputs |> circ clk rst ena)
 where
  (clk, rst, ena) = (C.clockGen, resetGen resetCycles, C.enableGen)

-- | Like 'C.resetGenN', but works on 'Int' instead of 'C.SNat'. Not
-- synthesizable.
resetGen :: C.KnownDomain dom => Int -> C.Reset dom
resetGen n = C.unsafeFromHighPolarity
  (C.fromList (replicate n True <> repeat False))
