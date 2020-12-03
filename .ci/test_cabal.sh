#!/bin/bash
set -xeou pipefail

cabal build all -fci
cabal run test-library -fci --enable-tests
cabal sdist
