#!/bin/bash
set -xeou pipefail

cabal build all -fci
cabal run unittests -fci --enable-tests
cabal sdist
