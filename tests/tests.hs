
module Main where

import Prelude
import Test.Tasty
import Control.Concurrent (setNumCapabilities)
import System.Environment (setEnv, lookupEnv)
import Text.Read (readMaybe)
import Control.Monad (join)

import qualified Tests.Protocols

main :: IO ()
main = do
  -- Hedgehog already tets in parallel
  setEnv "TASTY_NUM_THREADS" "2"

  -- Detect "THREADS" environment variable on CI
  nThreads <- join . fmap readMaybe <$> lookupEnv "THREADS"
  case nThreads of
    Nothing -> pure ()
    Just n -> do
      setNumCapabilities n

  defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ Tests.Protocols.tests
  ]
