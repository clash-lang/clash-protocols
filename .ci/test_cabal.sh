#!/bin/bash
set -xeou pipefail

cabal build all --constraint=clash-prelude==$clash_version  -fci
cabal run unittests --constraint=clash-prelude==$clash_version -fci --enable-tests
cabal run doctests --constraint=clash-prelude==$clash_version -fci --enable-tests
cabal sdist
