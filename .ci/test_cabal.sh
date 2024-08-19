#!/bin/bash
set -xeou pipefail

cabal v2-build all --constraint=clash-prelude==$clash_version  -fci
cabal v2-run unittests --constraint=clash-prelude==$clash_version -fci --enable-tests
cabal v2-run doctests --constraint=clash-prelude==$clash_version -fci --enable-tests
cabal v2-sdist clash-protocols
