module Main where

import Prelude
import Build_doctests (flags, pkgs, module_sources)
import Test.DocTest (doctest)
import System.Environment (lookupEnv)
import System.Process

getGlobalPackageDb :: IO String
getGlobalPackageDb = readProcess "ghc" ["--print-global-package-db"] ""

main :: IO ()
main = do
  inNixShell <-lookupEnv "IN_NIX_SHELL"
  extraFlags <-
    case inNixShell of
      Nothing -> pure []
      Just _ -> pure . ("-package-db="++) <$> getGlobalPackageDb

  let
    pluginFlags =
      [ "-fplugin", "GHC.TypeLits.KnownNat.Solver"
      , "-fplugin", "GHC.TypeLits.Normalise"
      , "-fplugin", "GHC.TypeLits.Extra.Solver" ]

  doctest (flags ++ extraFlags ++ pkgs ++ pluginFlags ++ module_sources)
