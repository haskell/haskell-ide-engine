#/usr/bin/env bash
set -e
cd elisp
cask
stack exec cask -- exec buttercup -L .
cd ..
