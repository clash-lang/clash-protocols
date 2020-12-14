#!/bin/bash
set -xeou pipefail

# Build dependencies first, so they don't end up in logs
cabal build \
  --enable-documentation \
  --allow-newer=circuit-notation:ghc \
  clash-protocols

# circuit-notation currently _compiles on 8.10, but isn't usable. The only
# other GHC version it supports is 8.6.5, but this GHC bundles a Haddock that
# cannot generate documentation for clash-prelude. Hence, we build docs with
# 8.10 and relax circuit-notation's ghc bounds
cabal haddock \
  --enable-documentation \
  --allow-newer=circuit-notation:ghc \
  clash-protocols \
  |& tee haddock_log

set +e
if grep -q "Missing documentation" haddock_log; then
  echo -e "\e[1m\e[31mMissing documentation! Scroll up for full log.\e[0m"
  grep --color=always -n -C 5 "Missing documentation" haddock_log
  exit 1
fi

if grep -q "If you qualify the identifier, haddock can try to link it anyway" haddock_log; then
  echo -e "\e[1m\e[31mIdentifier out of scope! Scroll up for full log.\e[0m"
  grep --color=always -n -C 5 "If you qualify the identifier, haddock can try to link it anyway" haddock_log
  exit 1
fi

# Copy documention to docs/
ln -s "$(dirname "$(tail -n1 haddock_log)")" docs
