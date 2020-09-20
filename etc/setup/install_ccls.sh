#!/bin/bash

current_dir="$(cd "$(dirname "${BASH_SOURCE[0]}" )" > /dev/null && pwd)"

# shellcheck source=./utils.sh
. "${current_dir}/utils.sh"

BUILD=

mkdir -p ~/.emacs.d/var
if cd ~/.emacs.d/var/ccls; then
    log "upgrade ccls"
    if git pull && git submodule update; then
        BUILD=1
    fi
else
    if cd ~/.emacs.d/var; then
        git clone --depth=1 --recursive https://github.com/MaskRay/ccls
        if cd ccls; then
            BUILD=1
        fi
    fi
fi

if [ -n $BUILD ]; then
    cmake -H. -Bbuild -DCMAKE_BUILD_TYPE=Release \
                  -DCMAKE_PREFIX_PATH=/usr/lib/llvm-10 \
                  -DLLVM_INCLUDE_DIR=/usr/lib/llvm-10/include \
                  -DLLVM_BUILD_INCLUDE_DIR=/usr/include/llvm-10/
    cmake --build build
fi
