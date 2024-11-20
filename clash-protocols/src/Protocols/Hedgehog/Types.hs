{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_HADDOCK hide #-}

-- These types should be re-exported from the Protocols.Hedgehog module
module Protocols.Hedgehog.Types where

-- deepseq
import Control.DeepSeq

import qualified Clash.Prelude as C
import Data.Proxy
import GHC.Stack (HasCallStack)
import Protocols.Internal.Types

-- hedgehog
import qualified Hedgehog as H

-- | Superclass class to reduce syntactical noise.
class (NFData a, C.NFDataX a, C.ShowX a, C.Show a, Eq a) => TestType a

instance (NFData a, C.NFDataX a, C.ShowX a, C.Show a, Eq a) => TestType a

-- | Options for 'expectN' function. See individual fields for more information.
data ExpectOptions = ExpectOptions
  { eoStopAfterEmpty :: Int
  -- ^ Stop sampling after seeing /n/ consecutive empty samples
  , eoSampleMax :: Int
  -- ^ Produce an error if the circuit produces more than /n/ valid samples. This
  -- is used to terminate (potentially) infinitely running circuits.
  --
  -- This number is used to generate stall information, so setting it to
  -- unreasonable values will result in long runtimes.
  , eoResetCycles :: Int
  -- ^ Ignore first /n/ cycles
  , eoDriveEarly :: Bool
  -- ^ Start driving the circuit with its reset asserted. Circuits should
  -- never acknowledge data while this is happening.
  , eoTimeoutMs :: Maybe Int
  -- ^ Terminate the test after /n/ milliseconds.
  , eoTrace :: Bool
  -- ^ Trace data generation for debugging purposes
  }

{- | Provides a way of comparing expected data with data produced by a
protocol component.
-}
class
  ( Drivable a
  , TestType (SimulateFwdType a)
  , TestType (ExpectType a)
  , -- Foldable requirement on Vec :(
    1 C.<= SimulateChannels a
  ) =>
  Test a
  where
  -- | Trim each channel to the lengths given as the third argument. See
  -- result documentation for failure modes.
  expectN ::
    (HasCallStack, H.MonadTest m) =>
    Proxy a ->
    -- | Options, see 'ExpectOptions'
    ExpectOptions ->
    -- | Raw sampled data
    SimulateFwdType a ->
    -- | Depending on "ExpectOptions", fails the test if:
    --
    --   * Circuit produced less data than expected
    --   * Circuit produced more data than expected
    --
    -- If it does not fail, /SimulateFwdType a/ will contain exactly the number
    -- of expected data packets.
    --
    -- TODO:
    --   Should probably return a 'Vec (SimulateChannels) Failures'
    --   in order to produce pretty reports.
    m (ExpectType a)
