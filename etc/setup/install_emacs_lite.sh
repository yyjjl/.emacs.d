#!/bin/bash

CURRENT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}" )" > /dev/null && pwd)"

. "${CURRENT_DIR}/utils.sh"

if [ -z "$1" ]; then
    emacs_VERSION=27.1
else
    emacs_VERSION="$1"
fi

READLINE_VERSION=6.3
NCURSES_VERSION=6.1
INSTALL_PREFIX=${HOME}/.local
PROGRAM_DIR=${HOME}/program

mkdir -p "${PROGRAM_DIR}/" && cd "${PROGRAM_DIR}/" || exit 1

compile_gnu_source() {
    local ext="$3"
    local dir="$1-$2"
    local url="https://mirrors.ustc.edu.cn/gnu/$1/${dir}.${ext}"
    local fmt="-x"

    if [ "${ext}" = "tar.gz" ]; then
        fmt="-zx"
    elif [ "${ext}" = "tar.xz" ]; then
        fmt="-Jx"
    fi

    info "Download and compile ${dir}"
    if [ -d "${dir}" ]; then
        info "${dir} exists."
    else
        info "wget ${url} ..."
        if ! wget -O- "${url}" | tar "${fmt}"; then
            critical "Cannot download ${dir} source !!!"
            rm -rf "${dir}"
            exit 1
        fi
        shift; shift; shift
        cd "${dir}" || exit 1
        if ! (./configure "$@" && nice make -j4 && make install);
        then
            critical "Cannot compile source !!!"
            rm -rf "${dir}"
            exit 1
        fi
        cd ..
    fi
}


compile_gnu_source ncurses ${NCURSES_VERSION} tar.gz \
                   --prefix="${INSTALL_PREFIX}"
compile_gnu_source readline ${READLINE_VERSION} tar.gz \
                   --prefix="${INSTALL_PREFIX}"

export LDFLAGS="-L${INSTALL_PREFIX}/lib"
compile_gnu_source emacs ${emacs_VERSION} tar.xz \
                   --prefix="${INSTALL_PREFIX}" \
                   --without-x --with-xpm=no --with-jpeg=no \
                   --with-png=no --with-gif=no --with-tiff=no \
                   --with-gnutls=no
