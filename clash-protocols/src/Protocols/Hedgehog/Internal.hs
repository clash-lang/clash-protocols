{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
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
import qualified Clash.Prelude as C

-- hedgehog

import qualified Data.Maybe as Maybe
import qualified Hedgehog as H
import qualified Hedgehog.Internal.Property as H

{- | Resets for 30 cycles, checks for superfluous data for 50 cycles after
seeing last valid data cycle, and times out after seeing 1000 consecutive
empty cycles.
-}
defExpectOptions :: ExpectOptions
defExpectOptions =
  ExpectOptions
    { -- XXX: These numbers are arbitrary, and should be adjusted to fit the
      --      protocol being tested. Annoyingly, upping these values will
      --      increase the time it takes to run the tests. This is because
      --      the test will run for at least the number of cycles specified
      --      in 'eoStopAfterEmpty'.
      eoStopAfterEmpty = 256
    , eoSampleMax = 256
    , eoResetCycles = 30
    , eoDriveEarly = True
    , eoTimeoutMs = Nothing
    , eoTrace = False
    , eoVcd = Nothing
    }

instance (TestType a, C.KnownDomain dom) => Test (Df dom a) where
  trimN ::
    forall m.
    (HasCallStack, H.MonadTest m) =>
    Proxy (Df dom a) ->
    ExpectOptions ->
    [Maybe a] ->
    m [Maybe a]
  trimN Proxy (ExpectOptions{eoSampleMax, eoStopAfterEmpty}) sampled = do
    go eoSampleMax eoStopAfterEmpty sampled
   where
    go :: (HasCallStack) => Int -> Int -> [Maybe a] -> m [Maybe a]
    go _timeout _n [] =
      -- This really should not happen, protocols should produce data indefinitely
      error "unexpected end of signal"
    go 0 _ _ =
      -- Sample limit reached
      H.failWith
        Nothing
        ( "Sample limit reached after sampling "
            <> show eoSampleMax
            <> " samples. "
            <> "Consider increasing 'eoSampleMax' in 'ExpectOptions'."
        )
    go _ 0 _ =
      -- Saw enough valid samples, return to user
      pure []
    go sampleTimeout _emptyTimeout (a : as) =
      -- Valid sample
      (a :) <$> go (sampleTimeout - 1) eoStopAfterEmpty as

  getExpectType ::
    Proxy (Df dom a) ->
    [Maybe a] ->
    [a]
  getExpectType Proxy = Maybe.catMaybes

instance
  ( Test a
  , C.KnownNat n
  , 1 <= (n * SimulateChannels a)
  , 1 <= n
  ) =>
  Test (C.Vec n a)
  where
  trimN ::
    forall m.
    (HasCallStack, H.MonadTest m) =>
    Proxy (C.Vec n a) ->
    ExpectOptions ->
    C.Vec n (SimulateFwdType a) ->
    m (C.Vec n (SimulateFwdType a))
  -- TODO: This creates some pretty terrible error messages, as one
  -- TODO: simulate channel is checked at a time.
  trimN Proxy opts = mapM (trimN (Proxy @a) opts)

  getExpectType ::
    Proxy (C.Vec n a) ->
    C.Vec n (SimulateFwdType a) ->
    C.Vec n (ExpectType a)
  getExpectType Proxy = fmap (getExpectType (Proxy @a))

instance
  ( Test a
  , Test b
  , 1 <= (SimulateChannels a + SimulateChannels b)
  ) =>
  Test (a, b)
  where
  trimN ::
    forall m.
    (HasCallStack, H.MonadTest m) =>
    Proxy (a, b) ->
    ExpectOptions ->
    (SimulateFwdType a, SimulateFwdType b) ->
    m (SimulateFwdType a, SimulateFwdType b)
  trimN Proxy opts (sampledA, sampledB) = do
    -- TODO: This creates some pretty terrible error messages, as one
    -- TODO: simulate channel is checked at a time.
    trimmedA <- trimN (Proxy @a) opts sampledA
    trimmedB <- trimN (Proxy @b) opts sampledB
    pure (trimmedA, trimmedB)

  getExpectType ::
    Proxy (a, b) ->
    (SimulateFwdType a, SimulateFwdType b) ->
    (ExpectType a, ExpectType b)
  getExpectType Proxy (simFwdA, simFwdB) =
    let
      expectFwdA = getExpectType (Proxy @a) simFwdA
      expectFwdB = getExpectType (Proxy @b) simFwdB
     in
      (expectFwdA, expectFwdB)

-- XXX: We only generate up to 9 tuples instead of maxTupleSize because NFData
-- instances are only available up to 9-tuples.
-- see https://hackage.haskell.org/package/deepseq-1.5.1.0/docs/src/Control.DeepSeq.html#line-1125
testTupleInstances 3 9
