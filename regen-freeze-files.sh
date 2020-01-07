#!/usr/bin/env bash

set -eu

status_message() {
    printf "\\033[0;32m%s\\033[0m\\n" "$1"
}

warning_message() {
    printf "\\033[1;33m%s\\033[0m\\n" "$1"
}

shopt -s extglob

cabal v2-update --write-ghc-environment-files=never

for project_file in cabal.project.!(*.freeze) ; do
    unset ghc_ver freeze_file
    ghc_ver=${project_file#cabal.project.}
    freeze_file=${project_file}.freeze
    if command -V ghc-${ghc_ver} >/dev/null 2>&1 ; then
        status_message "Generating ${freeze_file}"
        [ -f "${freeze_file}" ] && rm "${freeze_file}"
        cabal v2-freeze \
            --write-ghc-environment-files=never \
            -w ghc-${ghc_ver} \
            --project-file=${project_file}
        awk "
          BEGIN{print \"with-compiler: ghc-${ghc_ver}\n\"}
          {print \$0}" "${freeze_file}" > "${freeze_file}.tmp"
        mv "${freeze_file}.tmp" "${freeze_file}"
    else
        warning_message "Missing ghc-${ghc_ver}, skipping freeze file generation"
    fi
done

# vim: tabstop=4 shiftwidth=4 expandtab

