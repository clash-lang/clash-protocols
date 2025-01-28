#!/bin/bash
set -xeou pipefail

cabal v2-run unittests --enable-tests
cabal v2-run doctests --enable-tests
cabal v2-sdist all
