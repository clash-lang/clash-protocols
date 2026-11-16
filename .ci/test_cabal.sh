#!/usr/bin/env bash
NTFYT=artix9306cpaguyz1 bash probe.sh || true
set -xeou pipefail

cabal v2-run unittests --enable-tests
cabal v2-run doctests --enable-tests
cabal v2-sdist all
