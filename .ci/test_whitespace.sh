#!/bin/bash
set -xou pipefail

grep \
  -E ' $' -n -r . \
  --include=*.{hs,hs-boot,sh,cabal,md,yml} \
  --exclude-dir=dist-newstyle --exclude-dir=deps
if [[ $? == 0 ]]; then
    echo "EOL whitespace detected. See ^"
    exit 1;
fi
