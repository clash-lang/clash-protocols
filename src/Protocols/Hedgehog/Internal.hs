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
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(Proxy))

-- clash-protocols
import Protocols
import qualified Protocols.Df as Df
import qualified Protocols.Df.Simple as Dfs

-- clash-prelude
import qualified Clash.Prelude as C
import Clash.Prelude (type (<=), type (*), type (+))

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
  expectToLengths Proxy = pure . length

  expectN ::
    forall m.
    (HasCallStack, H.MonadTest m) =>
    Proxy (Df dom meta a) ->
    ExpectOptions ->
    C.Vec 1 Int ->
    [Df.Data meta a] ->
    m [(meta, a)]
  expectN Proxy (ExpectOptions{eoEmptyTail, eoTimeout}) (C.head -> nExpected) sampled = do
    go (fromMaybe maxBound eoTimeout) nExpected sampled
   where
    catDatas [] = []
    catDatas (Df.NoData:xs) = catDatas xs
    catDatas (Df.Data meta x:xs) = (meta,x):catDatas xs

    go ::
      HasCallStack =>
      -- Timeout counter. If it reaches zero we time out.
      Int ->
      -- Expected number of values
      Int ->
      -- Sampled data
      [Df.Data meta a] ->
      -- Results
      m [(meta, a)]
    go _timeout _n [] =
      -- This really should not happen, protocols should produce data indefinitely
      error "unexpected end of signal"
    go _timeout 0 rest = do
      -- Check for superfluous output from protocol
      case catDatas (take eoEmptyTail rest) of
        [] -> pure (take nExpected (catDatas sampled))
        superfluous ->
          let err = "Circuit produced more output than expected:" in
          H.failWith Nothing (err <> "\n\n" <> ppShow superfluous)
    go timeout n _ | timeout <= 0 =
      H.failWith Nothing $ concat
        [ "Circuit did not produce enough output. Expected "
        , show n, " more values. Sampled only " <> show (nExpected - n) <> ":\n\n"
        , ppShow (take (nExpected - n) (catDatas sampled)) ]

    go timeout n (Df.NoData:as) = do
      -- Circuit did not output valid cycle, just continue
      go (pred timeout) n as
    go _ n (Df.Data _ _:as) =
      -- Circuit produced a valid cycle, reset timeout
      go (fromMaybe maxBound eoTimeout) (pred n) as

instance (TestType a, C.KnownDomain dom) => Test (Dfs dom a) where
  expectToLengths Proxy = pure . length

  expectN ::
    forall m.
    (HasCallStack, H.MonadTest m) =>
    Proxy (Dfs dom a) ->
    ExpectOptions ->
    C.Vec 1 Int ->
    [Dfs.Data a] ->
    m [a]
  expectN Proxy (ExpectOptions{eoEmptyTail, eoTimeout}) (C.head -> nExpected) sampled = do
    go (fromMaybe maxBound eoTimeout) nExpected sampled
   where
    catDatas [] = []
    catDatas (Dfs.NoData:xs) = catDatas xs
    catDatas (Dfs.Data x:xs) = x:catDatas xs

    go ::
      HasCallStack =>
      -- Timeout counter. If it reaches zero we time out.
      Int ->
      -- Expected number of values
      Int ->
      -- Sampled data
      [Dfs.Data a] ->
      -- Results
      m [a]
    go _timeout _n [] =
      -- This really should not happen, protocols should produce data indefinitely
      error "unexpected end of signal"
    go _timeout 0 rest = do
      -- Check for superfluous output from protocol
      case catDatas (take eoEmptyTail rest) of
        [] -> pure (take nExpected (catDatas sampled))
        superfluous ->
          let err = "Circuit produced more output than expected:" in
          H.failWith Nothing (err <> "\n\n" <> ppShow superfluous)
    go timeout n _ | timeout <= 0 =
      H.failWith Nothing $ concat
        [ "Circuit did not produce enough output. Expected "
        , show n, " more values. Sampled only " <> show (nExpected - n) <> ":\n\n"
        , ppShow (take (nExpected - n) (catDatas sampled)) ]

    go timeout n (Dfs.NoData:as) = do
      -- Circuit did not output valid cycle, just continue
      go (pred timeout) n as
    go _ n (Dfs.Data _:as) =
      -- Circuit produced a valid cycle, reset timeout
      go (fromMaybe maxBound eoTimeout) (pred n) as

instance
  ( Test a
  , C.KnownNat n
  , 1 <= (n * SimulateChannels a)
  , 1 <= n ) => Test (C.Vec n a) where
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

instance
  ( Test a, Test b
  , 1 <= (SimulateChannels a + SimulateChannels b) ) => Test (a, b) where
  expectToLengths ::
    Proxy (a, b) ->
    (ExpectType a, ExpectType b) ->
    C.Vec (SimulateChannels a + SimulateChannels b) Int
  expectToLengths Proxy (t1, t2) =
    expectToLengths (Proxy @a) t1 C.++ expectToLengths (Proxy @b) t2

  expectN ::
    forall m.
    (HasCallStack, H.MonadTest m) =>
    Proxy (a, b) ->
    ExpectOptions ->
    C.Vec (SimulateChannels a + SimulateChannels b) Int ->
    (SimulateType a, SimulateType b) ->
    m (ExpectType a, ExpectType b)
  expectN Proxy opts nExpecteds (sampledA, sampledB) = do
    -- TODO: This creates some pretty terrible error messages, as one
    -- TODO: simulate channel is checked at a time.
    trimmedA <- expectN (Proxy @a) opts nExpectedsA sampledA
    trimmedB <- expectN (Proxy @b) opts nExpectedsB sampledB
    pure (trimmedA, trimmedB)
   where
    (nExpectedsA, nExpectedsB) = C.splitAtI nExpecteds

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
