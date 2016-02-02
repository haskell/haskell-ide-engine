#/usr/bin/env bash
set -e
DIR=$(pwd)
cd elisp
cask
HIEBASE="$DIR" stack exec cask -- exec buttercup -L .
cd ..
