#!/usr/bin/env bash
set -e # Exit on first error

cabal build 2>&1
cabal exec website -- rebuild 2>&1
cabal exec website -- watch 2>&1