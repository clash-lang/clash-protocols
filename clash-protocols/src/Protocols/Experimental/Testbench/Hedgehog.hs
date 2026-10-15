{-# LANGUAGE OverloadedStrings #-}

{- |
Hedgehog integration for testbenches. The harness itself does not generate any
stimuli: generate data and options with 'Hedgehog.forAll' in your property, build
a testbench, and check it with 'assertTestbench'.
-}
module Protocols.Experimental.Testbench.Hedgehog (
  assertTestbench,
) where

import Control.Monad (forM_, unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.String (fromString)
import GHC.Stack (HasCallStack, withFrozenCallStack)
import Hedgehog (MonadTest)
import Protocols.Experimental.Testbench.Coverage (CoverGoal (..))
import Protocols.Experimental.Testbench.Run
import Protocols.Experimental.Testbench.Trace (TraceTree)
import Protocols.Plugin (Circuit)

import Data.List qualified as L
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Hedgehog qualified as H

{- | Run a testbench, and fail the property if the testbench does not pass. On
failure, the error, the time it occurred, and the path of the waveform (if
any, see t'TestOptions') are reported.

Every cover point of the testbench is reported to Hedgehog with 'H.cover'.
Hedgehog adds these up over all runs of a property, shows them in a table, and
fails the property if a t'CoverGoal' is not met. Cover points without a goal are
reported with a goal of 0%. A goal for a cover point that does not exist fails
the property.
-}
assertTestbench ::
  (MonadIO m, MonadTest m, HasCallStack) =>
  TestOptions ->
  [CoverGoal] ->
  Circuit () TraceTree ->
  m ()
assertTestbench opts goals tb = withFrozenCallStack $ do
  result <- liftIO (runTestbench opts tb)

  let
    time = "at " <> show result.endTime <> " fs"
    vcdNote = case result.vcdFile of
      Nothing -> "No waveform written, see 'vcdOutput' in t'TestOptions'."
      Just path -> "Waveform written to: " <> path

  case result.outcome of
    Passed -> pure ()
    Failed{source, message} -> do
      H.footnote vcdNote
      H.footnote $
        "Testbench failed "
          <> time
          <> (if Text.null source then "" else ", reported by " <> Text.unpack source)
          <> ":\n"
          <> Text.unpack message
      H.failure
    TimedOut{blockedBy} -> do
      H.footnote vcdNote
      H.footnote $
        "Testbench timed out "
          <> time
          <> ". Still waiting for: "
          <> L.intercalate ", " (Text.unpack <$> blockedBy)
      H.failure

  let
    goalMap = Map.fromList [(g.path, g.minRuns) | g <- goals]
    unknown = Map.keys (Map.difference goalMap result.coverage)

  unless (L.null unknown) $ do
    H.footnote $
      "Known cover points: " <> L.intercalate ", " (Text.unpack <$> Map.keys result.coverage)
    H.footnote $
      "Coverage goals for unknown cover points: "
        <> L.intercalate ", " (Text.unpack <$> unknown)
    H.failure

  forM_ (Map.toList result.coverage) $ \(path, hits) ->
    H.cover (Map.findWithDefault 0 path goalMap) (fromString (Text.unpack path)) (hits > 0)
