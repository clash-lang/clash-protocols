{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Protocols.Test.Samples where

import Clash.Explicit.Prelude hiding (bitSize)

import Clash.Signal.Internal (Clock (..), Femtoseconds (..), Signal((:-)))
import Clash.Sized.Internal.BitVector (BitVector (..))
import Data.List.Infinite (Infinite)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Protocols.Internal (CSignal, Circuit (..), Protocol (..))

import qualified Data.List as L
import qualified Data.List.Infinite as I
import qualified Data.List.NonEmpty as NonEmpty

data Sample = Sample
  { value :: !Natural
  , mask :: !Natural
  }
  deriving (Eq, Show)

data Samples = Samples
  { name :: Text
  -- ^ Name of the signal
  , bitSize :: Natural
  -- ^ Size of the signal element in bits
  , period :: Maybe Femtoseconds
  -- ^ Period of the clock. Set if dealing with classical clocks (i.e., non-dynamic).
  , samples :: Infinite (Femtoseconds, Sample)
  -- ^ Samples with their timestamps
  }

instance Protocol Samples where
  type Fwd Samples = Samples
  type Bwd Samples = ()

-- | Record a signal to 'Samples'.
toSamplesIC ::
  forall a dom.
  ( BitPack a
  , KnownDomain dom
  , NFDataX a
  ) =>
  -- | Name of the signal
  Text ->
  -- | Clock, used to calculate time
  Clock dom ->
  -- | Circuit recording the signal to a "DriverSamples"
  Circuit (CSignal dom a) Samples
toSamplesIC name clock =
  Circuit ((pure (),) . toSamples name clock . fst)

-- | Record a signal to 'Samples'.
toSamples ::
  forall a dom.
  ( BitPack a
  , KnownDomain dom
  , NFDataX a
  ) =>
  -- | Name of the signal
  Text ->
  -- | Clock, used to calculate time
  Clock dom ->
  Signal dom a ->
  Samples
toSamples name (Clock{clockPeriods}) sig = Samples
  { name
  , bitSize = natToNum @(BitSize a)
  , period
  , samples = I.zip timestamps (packToSample <$> signalToInfinite sig)
  }
 where
  staticPeriod = Femtoseconds $ 1000 * snatToNum (clockPeriod @dom)

  timestamps = case clockPeriods of
    Nothing -> I.map (mulFs staticPeriod) (I.iterate (+1) 0)
    Just periods -> I.scanl addFs (Femtoseconds 0) (signalToInfinite periods)

  period = case clockPeriods of
    Nothing -> Just staticPeriod
    Just _ -> Nothing

  addFs (Femtoseconds fs0) (Femtoseconds fs1) = Femtoseconds (fs0 + fs1)
  mulFs (Femtoseconds fs) n = Femtoseconds (fs * n)

-- | Record a signal to 'Samples'.
toSamplesI ::
  forall a dom.
  ( BitPack a
  , NFDataX a
  , KnownDomain dom) =>
  -- | Name of the signal
  Text ->
  Signal dom a ->
  Samples
toSamplesI name = toSamples name (clockGen @dom)

data TestOptions = TestOptions
  { trace :: Bool
  -- ^ Whether to produce a trace file
  , timeout :: Maybe Femtoseconds
  -- ^ Timeout in simulation time
  }
  deriving (Eq, Show)

-- | Convert a bunch of streams (paired with arbitrary IDs) to a priority queue. As
-- long as the streams have monotonically increasing timestamps, so will the result.
--
-- TODO: Allow grouping of samples with the same timestamps.
--
-- TODO: Work on clocks instead of Femtoseconds? I.e., do what @clash-prelude@ does.
streamsToOrderedSamples ::
  forall id sample.
  NonEmpty (id, Infinite (Femtoseconds, sample)) ->
  Infinite (Femtoseconds, NonEmpty (id, sample))
streamsToOrderedSamples = go1
 where
  -- Find minimum time in next samples
  go1 ::
    NonEmpty (id, Infinite (Femtoseconds, sample)) ->
    Infinite (Femtoseconds, NonEmpty (id, sample))
  go1 samples = go2 (L.minimum (fst . I.head . snd <$> samples)) samples

  -- Pop items with given time
  go2 ::
    Femtoseconds ->
    NonEmpty (id, Infinite (Femtoseconds, sample)) ->
    Infinite (Femtoseconds, NonEmpty (id, sample))
  go2 now samples = (now, NonEmpty.fromList popped) I.:< go1 rest
    where
      (popped, rest) = L.mapAccumL pop [] samples
      pop acc prev@(id_, I.uncons -> ((timestamp, s), as))
        | timestamp == now = ((id_, s) : acc, (id_, as))
        | otherwise = (acc, prev)

-- | Convert a 'Signal' to its (isomorphic) 'Infinite' representation
signalToInfinite :: Signal dom a -> Infinite a
signalToInfinite = go
 where
  go :: Signal dom a -> Infinite a
  go (s :- xs) = s I.:< go xs

-- | Convert a 'BitPack'able thing to a 'Sample'
packToSample :: (BitPack a) => a -> Sample
packToSample (pack -> BV{unsafeMask, unsafeToNatural}) = Sample
  { value = unsafeToNatural
  , mask = unsafeMask
  }
