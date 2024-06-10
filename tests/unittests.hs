module Main where

import Control.Concurrent (setNumCapabilities)
import Control.Monad (join)
import System.Environment (lookupEnv, setEnv)
import Test.Tasty
import Text.Read (readMaybe)
import Prelude

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
tests =
  testGroup
    "Tests"
    [ Tests.Protocols.tests
    ]
