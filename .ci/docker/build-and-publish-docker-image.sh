#!/usr/bin/env bash

set -xeo pipefail

REPO="docker.pkg.github.com/clash-lang/clash-protocols"
DIR=$(dirname "$0")
now=$(date +%F)

for GHC_VERSION in "8.6.5" "8.10.2"
do
  NAME="protocols-focal-ghc-cabal-stack-${GHC_VERSION}"

  docker build -t "${REPO}/${NAME}:$now" "$DIR" --build-arg GHC_VERSION=${GHC_VERSION}
  docker tag "${REPO}/${NAME}:$now" "${REPO}/${NAME}:latest"

  read -p "Push to GitHub? (y/N) " push

  if [[ $push =~ ^[Yy]$ ]]; then
          docker push "${REPO}/${NAME}:$now"
          docker push "${REPO}/${NAME}:latest"
  else
          echo "Skipping push to container registry"
  fi
done
