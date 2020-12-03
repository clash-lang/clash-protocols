#!/bin/bash
set -xeou pipefail

cabal haddock --enable-documentation clash-protocols |& tee haddock_log

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
