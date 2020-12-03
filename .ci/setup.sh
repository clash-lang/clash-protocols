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

set -e

ghcup set ghc ${GHC_VERSION}
ghcup set cabal ${CABAL_VERSION}

cabal --version
ghc --version

# run new-update first to generate the cabal config file that we can then modify
# retry 5 times, as hackage servers are not perfectly reliable
NEXT_WAIT_TIME=0
until cabal update || [ $NEXT_WAIT_TIME -eq 5 ]; do
   sleep $(( NEXT_WAIT_TIME++ ))
done
