#!/usr/bin/env bash

current_dir="$(cd "$(dirname "${BASH_SOURCE[0]}" )" > /dev/null && pwd)"

# shellcheck source=./utils.sh
. "${current_dir}/utils.sh"

BUILD=

if  [ -d "~/.emacs.d/var/translate-shell" ]; then
    log "upgrade translate-shell"
    if git pull && git submodule update; then
        BUILD=1
    fi
else
    if cd ~/.emacs.d/var ; then
        if git clone --depth=1 https://github.com/soimort/translate-shell; then
            BUILD=1
        fi
    fi
fi

if [ -n $BUILD ]; then
    make -C translate-shell
fi
