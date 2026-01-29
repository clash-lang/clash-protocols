{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK hide #-}

{- |
Internals for "Protocols.Hedgehog".
-}
module Protocols.Hedgehog.Internal (
  module Protocols.Hedgehog.Internal,
  module Protocols.Hedgehog.Types,
) where

-- base
import Data.Proxy (Proxy (Proxy))
import GHC.Stack (HasCallStack)
import Prelude

-- clash-protocols
import Protocols
import Protocols.Hedgehog.Types
import Protocols.Internal.TH

-- clash-prelude
import Clash.Prelude (type (*), type (+), type (<=))
import Clash.Prelude qualified as C

-- hedgehog
import Hedgehog qualified as H
import Hedgehog.Internal.Property qualified as H

{- | Conservative settings for `ExpectOptions`:
- Reset for 30 cycles
- Insert at most 10 stall moments
- Every stall moment is at most 10 cycles long
- Sample at most 1000 cycles
- Automatically derive when to stop sampling empty samples using `expectedEmptyCycles`.
-}
defExpectOptions :: ExpectOptions
defExpectOptions =
  ExpectOptions
    { -- XXX: These numbers are arbitrary, and should be adjusted to fit the
      --      protocol being tested. Annoyingly, upping these values will
      --      increase the time it takes to run the tests. This is because
      --      the test will run for at least the number of cycles specified
      --      in 'eoStopAfterEmpty'.
      eoStopAfterEmpty = Nothing
    , eoSampleMax = 1000
    , eoStallsMax = 10
    , eoConsecutiveStalls = 10
    , eoResetCycles = 30
    , eoDriveEarly = True
    , eoTimeoutMs = Nothing
    , eoTrace = False
    }

instance (TestType a, C.KnownDomain dom) => Test (Df dom a) where
  expectN ::
    forall m.
    (HasCallStack, H.MonadTest m) =>
    Proxy (Df dom a) ->
    ExpectOptions ->
    [Maybe a] ->
    m [a]
  expectN Proxy eOpts sampled = do
    go eOpts.eoSampleMax maxEmptyCycles sampled
   where
    maxEmptyCycles = expectedEmptyCycles eOpts
    go :: (HasCallStack) => Int -> Int -> [Maybe a] -> m [a]
    go _timeout _n [] =
      -- This really should not happen, protocols should produce data indefinitely
      error "unexpected end of signal"
    go 0 _ _ =
      -- Sample limit reached
      H.failWith
        Nothing
        ( "Sample limit reached after sampling "
            <> show eOpts.eoSampleMax
            <> " samples. "
            <> "Consider increasing 'eoSampleMax' in 'ExpectOptions'."
        )
    go _ 0 _ =
      -- Saw enough valid samples, return to user
      pure []
    go sampleTimeout _emptyTimeout (Just a : as) =
      -- Valid sample
      (a :) <$> go (sampleTimeout - 1) maxEmptyCycles as
    go sampleTimeout emptyTimeout (Nothing : as) =
      -- Empty sample
      go sampleTimeout (emptyTimeout - 1) as

instance
  ( Test a
  , C.KnownNat n
  , 1 <= (n * SimulateChannels a)
  , 1 <= n
  ) =>
  Test (C.Vec n a)
  where
  expectN ::
    forall m.
    (HasCallStack, H.MonadTest m) =>
    Proxy (C.Vec n a) ->
    ExpectOptions ->
    C.Vec n (SimulateFwdType a) ->
    m (C.Vec n (ExpectType a))
  -- TODO: This creates some pretty terrible error messages, as one
  -- TODO: simulate channel is checked at a time.
  expectN Proxy opts = mapM (expectN (Proxy @a) opts)

instance Test () where
  expectN _ _ _ = pure ()

instance
  ( Test a
  , Test b
  , 1 <= (SimulateChannels a + SimulateChannels b)
  ) =>
  Test (a, b)
  where
  expectN ::
    forall m.
    (HasCallStack, H.MonadTest m) =>
    Proxy (a, b) ->
    ExpectOptions ->
    (SimulateFwdType a, SimulateFwdType b) ->
    m (ExpectType a, ExpectType b)
  expectN Proxy opts (sampledA, sampledB) = do
    -- TODO: This creates some pretty terrible error messages, as one
    -- TODO: simulate channel is checked at a time.
    trimmedA <- expectN (Proxy @a) opts sampledA
    trimmedB <- expectN (Proxy @b) opts sampledB
    pure (trimmedA, trimmedB)

-- XXX: We only generate up to 9 tuples instead of maxTupleSize because NFData
-- instances are only available up to 9-tuples.
-- see https://hackage.haskell.org/package/deepseq-1.5.1.0/docs/src/Control.DeepSeq.html#line-1125
testTupleInstances 3 9
