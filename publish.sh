#!/usr/bin/env bash
set -e # Exit on first error

# First make sure the site is rebuild
bash clean-build.sh

b146="jhaugh@b146-53.cs.unm.edu"

rsync -Pavz -e ssh "/home/jhaugh/teaching/website/docs/" "$b146:~/public_html/"