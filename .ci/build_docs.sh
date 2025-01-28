#!/bin/bash
set -xeou pipefail

cabal v2-haddock all |& tee haddock_log

set +e

suppressed_warnings=(
  'Consider exporting it together with its parent(s) for code clarity.'
  )

grep -v -e "${suppressed_warnings[@]}" haddock_log |& tee haddock_filtered

if grep -q "Missing documentation" haddock_filtered; then
  echo -e "\e[1m\e[31mMissing documentation! Scroll up for full log.\e[0m"
  grep --color=always -n -C 5 "Missing documentation" haddock_filtered
  exit 1
fi

if grep -q "If you qualify the identifier, haddock can try to link it anyway" haddock_filtered; then
  echo -e "\e[1m\e[31mIdentifier out of scope! Scroll up for full log.\e[0m"
  grep --color=always -n -C 5 "If you qualify the identifier, haddock can try to link it anyway" haddock_filtered
  exit 1
fi

if grep -q "could not find link destinations for" haddock_filtered; then
  echo -e "\e[1m\e[31mAn identifier could not be linked! Scroll up for full log.\e[0m"
  grep --color=always -n -C 5 "could not find link destinations for" haddock_filtered
  exit 1
fi

if grep -E -q "^Warning:" haddock_filtered; then
  echo -e "\e[1m\e[31mAn unknown warning occured. Scroll up for full log.\e[0m"
  grep --color=always -n -C 5 -E "^Warning:" haddock_filtered
  exit 1
fi

# Copy documention to docs/
ln -s "$(dirname "$(tail -n1 haddock_filtered)")" docs
