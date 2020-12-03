#!/bin/bash
set -xeou pipefail

stack build
stack test
