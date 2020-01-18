#!/usr/bin/env bash

set -eu

status_message() {
    printf "\\033[0;32m%s\\033[0m\\n" "$1"
}

error_message() {
    printf "\\033[0;31m%s\\033[0m\\n" "$1"
}

SCRIPTPATH="$( cd "$(dirname "$0")" ; pwd -P )"
CACHE_LOCATION="${HOME}/.cabal/packages/hackage.haskell.org/01-index.cache"

if [ ! -f "${CACHE_LOCATION}" ] ; then
	error_message "${CACHE_LOCATION} does not exist, did you run 'cabal update'?"
	exit 1
fi

if [ ! -f "${SCRIPTPATH}/cabal.project" ] ; then
	error_message "Could not find ${SCRIPTPATH}/cabal.project, skipping index state update."
	exit 3
fi

cabal v2-update

arch=$(getconf LONG_BIT)

case "${arch}" in
32)
	byte_size=4
	magic_word="CABA1002"
    ;;
64)
	byte_size=8
	magic_word="00000000CABA1002"
    ;;
*)
	error_message "Unknown architecture (long bit): ${arch}"
	exit 2
    ;;
esac

# This is the logic to parse the binary format of 01-index.cache.
# The first word is a magic 'caba1002', the second one is the timestamp in unix epoch.
# Better than copying the cabal-install source code.
if [ "$(xxd -u -p -l${byte_size} -s 0 "${CACHE_LOCATION}")" != "${magic_word}" ] ; then
	error_message "Magic word does not match!"
	exit 4
fi
cache_timestamp=$(echo "ibase=16;obase=A;$(xxd -u -p -l${byte_size} -s ${byte_size} "${CACHE_LOCATION}")" | bc)

# If we got junk from the binary file, this should fail.
cache_date=$(date --utc --date "@${cache_timestamp}" "+%FT%TZ")


status_message "Updating index state in ${SCRIPTPATH}/cabal.project"

if grep -q "^index-state: .*" "${SCRIPTPATH}/cabal.project" ; then
	awk '/index-state:/ {gsub(/.*/, "index-state: '${cache_date}'")}; { print }' "${SCRIPTPATH}/cabal.project" > "${SCRIPTPATH}/cabal.project.tmp"
	mv "${SCRIPTPATH}/cabal.project.tmp" "${SCRIPTPATH}/cabal.project"
else
	printf "index-state: %s\n" "${cache_date}" >> "${SCRIPTPATH}/cabal.project"
fi

