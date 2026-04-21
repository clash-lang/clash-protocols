#!/bin/bash
set -xeou pipefail

cabal v2-run clash-protocols:unittests --enable-tests
cabal v2-run clash-protocols-experimental:unittests --enable-tests

cabal v2-run clash-protocols:doctests --enable-tests
cabal v2-run clash-protocols-experimental:doctests --enable-tests

cabal v2-sdist all
