{-|
Internals for "Protocols.Hedgehog".
-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Protocols.Hedgehog.Internal where

-- base
import Prelude
import GHC.Stack (withFrozenCallStack, HasCallStack)
import Control.Monad (forM)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Kind (Type)
import Data.Proxy (Proxy(Proxy))

-- clash-protocols
import Protocols
import Protocols.Df (Df)
import Protocols.Df.Simple (Dfs)

-- clash-prelude
import qualified Clash.Prelude as C
import Clash.Prelude (type (<=), type (*))

-- deepseq
import Control.DeepSeq

-- hedgehog
import qualified Hedgehog as H
import qualified Hedgehog.Internal.Show as H
import qualified Hedgehog.Internal.Property as H

-- pretty-show
import Text.Show.Pretty (ppShow)

-- | Options for 'expectN' function. See individual fields for more information.
data ExpectOptions = ExpectOptions
  { -- | Sample /n/ cycles after last expected value and check for emptiness
    eoEmptyTail :: Int

    -- | Timeout after seeing /n/ empty cycles
  , eoTimeout :: Maybe Int

    -- | Ignore first /n/ cycles
  , eoResetCycles :: Int

    -- | Start driving the circuit with its reset asserted. Circuits should
    -- never acknowledge data while this is happening.
  , eoDriveEarly :: Bool
  }

-- | Resets for 30 cycles, checks for superfluous data for 50 cycles after
-- seeing last valid data cycle, and times out after seeing 1000 consecutive
-- empty cycles.
defExpectOptions :: ExpectOptions
defExpectOptions = ExpectOptions
  { eoEmptyTail = 50
  , eoTimeout = Just 1000
  , eoResetCycles = 30
  , eoDriveEarly = True
  }

-- | Superclass class to reduce syntactical noise.
class (NFData a, C.NFDataX a, C.ShowX a, C.Show a, Eq a) => TestType a
instance (NFData a, C.NFDataX a, C.ShowX a, C.Show a, Eq a) => TestType a

-- | Provides a way of comparing expected data with data produced by a
-- protocol component.
class ( Simulate a
      , TestType (SimulateType a)
      , TestType (ExpectType a)

      -- Foldable requirement on Vec :(
      , 1 <= SimulateChannels a
      ) => Test a where
  type ExpectType a :: Type

  -- | Convert a /ExpectType a/, a type representing data without backpressure,
  -- into a type that does, /SimulateType a/.
  expectToSimulateType ::
    -- | Type witness
    Proxy a ->
    -- | Expect type: input for a protocol /without/ stall information
    ExpectType a ->
    -- | Expect type: input for a protocol /with/ stall information
    SimulateType a

  -- | Get the number of expected valid data cycles for each data channel,
  -- given a list of expected data.
  expectToLengths ::
    Proxy a ->
    ExpectType a ->
    C.Vec (SimulateChannels a) Int

  -- | Trim each channel to the lengths given as the third argument. See
  -- result documentation for failure modes.
  expectN ::
    (HasCallStack, H.MonadTest m) =>
    Proxy a ->
    -- | Options, see 'ExpectOptions'
    ExpectOptions ->
    -- | Number of valid data cycles expected on each channel
    C.Vec (SimulateChannels a) Int ->
    -- | Raw sampled data
    SimulateType a ->
    -- | Depending on "ExpectOptions", fails the test if:
    --
    --   * Circuit produced less data than expected
    --   * Circuit produced more data than expected
    --
    -- If it does not fail, /SimulateType a/ will contain exactly the number
    -- of expected data packets.
    --
    -- TODO:
    --   Should probably return a 'Vec (SimulateChannels) Failures'
    --   in order to produce pretty reports.
    m (ExpectType a)

instance (TestType meta, TestType a, C.KnownDomain dom) => Test (Df dom meta a) where
  type ExpectType (Df dom meta a) = [(meta, a)]

  expectToSimulateType Proxy = map Just
  expectToLengths Proxy = pure . length
  expectN Proxy = expectN (Proxy @(Dfs dom (meta, a)))

instance (TestType a, C.KnownDomain dom) => Test (Dfs dom a) where
  type ExpectType (Dfs dom a) = [a]

  expectToSimulateType Proxy = map Just
  expectToLengths Proxy = pure . length

  expectN ::
    forall m.
    (HasCallStack, H.MonadTest m) =>
    Proxy (Dfs dom a) ->
    ExpectOptions ->
    C.Vec 1 Int ->
    [Maybe a] ->
    m [a]
  expectN Proxy (ExpectOptions{eoEmptyTail, eoTimeout}) (C.head -> nExpected) sampled = do
    go (fromMaybe maxBound eoTimeout) nExpected sampled
   where
    go ::
      (HasCallStack, Eq v, Show v) =>
      -- Timeout counter. If it reaches zero we time out.
      Int ->
      -- Expected number of values
      Int ->
      -- Sampled data
      [Maybe v] ->
      -- Results
      m [a]
    go _timeout _n [] =
      -- This really should not happen, protocols should produce data indefinitely
      error "unexpected end of signal"
    go _timeout 0 rest = do
      -- Check for superfluous output from protocol
      case catMaybes (take eoEmptyTail rest) of
        [] -> pure (take nExpected (catMaybes sampled))
        superfluous ->
          let err = "Circuit produced more output than expected:" in
          H.failWith Nothing (err <> "\n\n" <> ppShow superfluous)
    go timeout n _ | timeout <= 0 =
      H.failWith Nothing $ concat
        [ "Circuit did not produce enough output. Expected "
        , show n, " more values. Sampled only " <> show (nExpected - n) <> ":\n\n"
        , ppShow (take (nExpected - n) (catMaybes sampled)) ]

    go timeout n (Nothing:as) = do
      -- Circuit did not output valid cycle, just continue
      go (pred timeout) n as
    go _ n (Just _:as) =
      -- Circuit produced a valid cycle, reset timeout
      go (fromMaybe maxBound eoTimeout) (pred n) as

instance
  ( Test a
  , C.KnownNat n
  , 1 <= (n * SimulateChannels a)
  , 1 <= n ) => Test (C.Vec n a) where
  type ExpectType (C.Vec n a) = C.Vec n (ExpectType a)

  expectToSimulateType ::
    Proxy (C.Vec n a) ->
    C.Vec n (ExpectType a) ->
    C.Vec n (SimulateType a)
  expectToSimulateType Proxy =
    C.map (expectToSimulateType (Proxy @a))

  expectToLengths ::
    Proxy (C.Vec n a) ->
    ExpectType (C.Vec n a) ->
    C.Vec (n * SimulateChannels a) Int
  expectToLengths Proxy =
    C.concatMap (expectToLengths (Proxy @a))

  expectN ::
    forall m.
    (HasCallStack, H.MonadTest m) =>
    Proxy (C.Vec n a) ->
    ExpectOptions ->
    C.Vec (n * SimulateChannels a) Int ->
    C.Vec n (SimulateType a) ->
    m (C.Vec n (ExpectType a))
  expectN Proxy opts nExpecteds sampled = do
    -- TODO: This creates some pretty terrible error messages, as one
    -- TODO: simulate channel is checked at a time.
    forM
      (C.zip (C.unconcatI nExpecteds) sampled)
      (uncurry (expectN (Proxy @a) opts))

-- | Fails with an error that shows the difference between two values.
failDiffWith ::
  (H.MonadTest m, Show a, Show b, HasCallStack) =>
  -- | Additional info for error message
  String ->
  -- | Expected
  a ->
  -- | Actual
  b ->
  m ()
failDiffWith msg x y =
  case H.valueDiff <$> H.mkValue x <*> H.mkValue y of
    Nothing ->
      withFrozenCallStack $
        H.failWith Nothing $
        unlines $ [
            msg
          , "━━ expected ━━"
          , H.showPretty x
          , "━━ actual ━━"
          , H.showPretty y
          ]

    Just vdiff@(H.ValueSame _) ->
      withFrozenCallStack $
        H.failWith
          (Just $ H.Diff "━━━ Failed ("  "" "no differences" "" ") ━━━" vdiff)
          msg

    Just vdiff ->
      withFrozenCallStack $
        H.failWith
          (Just $ H.Diff "━━━ Failed (" "- expected" ") (" "+ actual" ") ━━━" vdiff)
          msg
