#!/bin/bash
set -xeou pipefail

stack --version
stack build
stack test
