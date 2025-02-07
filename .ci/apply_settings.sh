#!/bin/bash

set -uo pipefail

if [[ "$check_haddock" != @(True|False) ]]; then
  echo "check_haddock: Expected True or False, got \"$check_haddock\"" >&2
  exit 1
fi
sed <.ci/cabal.project.local.in >cabal.project.local "
    s/__CHECK_HADDOCK__/$check_haddock/
    s/__CLASH_VERSION__/$clash_version/"
