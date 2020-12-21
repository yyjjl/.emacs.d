#!/usr/bin/env bash

CURRENT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}" )" > /dev/null && pwd)"

. "${CURRENT_DIR}/utils.sh"

EMACS_VERSION=27.1
READLINE_VERSION=6.3
NCURSES_VERSION=6.1
PROGRAM_DIR=${HOME}/program

INSTALL_PREFIX=${HOME}/.local

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

compile_gnu_source emacs ${EMACS_VERSION} tar.xz \
                       --prefix="${INSTALL_PREFIX}" \
                       --with-all --with-x --with-pop --with-modules --with-gconf
