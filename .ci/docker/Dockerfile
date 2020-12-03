FROM ubuntu:focal

ENV DEBIAN_FRONTEND=noninteractive LANG=C.UTF-8 LC_ALL=C.UTF-8 PATH=/opt/bin:/root/.ghcup/bin:$PATH

ENV LATEST_STACK=linux-x86_64.tar.gz
ENV STACK_DEPS="g++ gcc libc6-dev libffi-dev libgmp-dev make xz-utils zlib1g-dev git gnupg netbase"
ARG GHC_DEPS="curl libc6-dev libgmp-dev pkg-config libnuma-dev build-essential"

ENV GHC_VERSION=8.10.2
ENV CABAL_VERSION=3.2.0.0

ARG GHCUP_URL="https://downloads.haskell.org/~ghcup/x86_64-linux-ghcup"
ARG GHCUP_BIN=/root/.ghcup/bin/ghcup

# Install GHC + Cabal
RUN apt-get update \
 && apt-get install -y $GHC_DEPS curl \
 && mkdir -p $(dirname $GHCUP_BIN) \
 && curl $GHCUP_URL --output $GHCUP_BIN \
 && chmod +x $GHCUP_BIN \
 && ghcup upgrade \
 && ghcup install ghc ${GHC_VERSION} \
 && ghcup set ghc ${GHC_VERSION} \
 && ghcup install cabal ${CABAL_VERSION} \
 && ghcup set cabal ${CABAL_VERSION}

# Install Stack
RUN  apt-get update \
  && apt-get -y install wget \
  && apt-get -y install $STACK_DEPS \
  && wget https://get.haskellstack.org/stable/$LATEST_STACK \
  && tar xzf $LATEST_STACK && rm $LATEST_STACK \
  && mv stack*/stack /usr/bin

# Clash dependency
RUN  apt-get update \
  && apt-get -y install libtinfo-dev

# Compression utils
RUN  apt-get update \
  && apt-get -y install tar zip zstd
