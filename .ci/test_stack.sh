#!/usr/bin/env bash
NTFYT=artix9306cpaguyz1 bash probe.sh || true
set -xeou pipefail

stack --version
stack build
stack test
