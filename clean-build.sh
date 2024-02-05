#!/usr/bin/env bash
set -e # Exit on first error

stack build 2>&1
stack exec website -- rebuild 2>&1

# If 1 argument then also watch
if [ "$#" -eq 1 ]; then
    stack exec website -- watch 2>&1
fi
