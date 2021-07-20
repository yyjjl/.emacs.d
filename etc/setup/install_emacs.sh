#!/bin/bash

CURRENT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}" )" > /dev/null && pwd)"

. "${CURRENT_DIR}/utils.sh"

PROGRAM_DIR=${HOME}/program
INSTALL_PREFIX=${HOME}/.local

mkdir -p "${PROGRAM_DIR}/" && cd "${PROGRAM_DIR}/" || exit 1

cd "${PROGRAM_DIR}"
if [ ! -d "emacs" ]; then
    git clone --depth=1 https://github.com/emacs-mirror/emacs emacs
fi
cd emacs
./autogen.sh
./configure \
   --prefix="${INSTALL_PREFIX}" \
   --with-json \
   --with-all --with-x --with-modules --with-gconf
   --with-native-compilation
make -j && make install
