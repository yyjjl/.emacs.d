#!/bin/bash

CURRENT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}" )" > /dev/null && pwd)"

. "${CURRENT_DIR}/utils.sh"

NETTLE_VERSION=2.7
GNUTLS_VERSION=3.1.5
GMP_VERSION=6.2.0
TEXINFO_VERSION=6.8
READLINE_VERSION=6.3
NCURSES_VERSION=6.1
INSTALL_PREFIX=${HOME}/.local
PROGRAM_DIR=${HOME}/program

mkdir -p "${PROGRAM_DIR}/" && cd "${PROGRAM_DIR}/" || exit 1

compile_gnu_source() {
    local ext="$3"
    local dir="$1-$2"
    local url="https://ftp.gnu.org/gnu/$1/${dir}.${ext}"
    local fmt="-x"
    shift; shift; shift

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
    fi

    cd "${dir}" || exit 1
    if ! (./configure --prefix="${INSTALL_PREFIX}" "$@" && nice make -j4 && make install); then
        critical "Cannot compile source !!!"
        exit 1
    fi
    cd ..
}

export LDFLAGS="-L${INSTALL_PREFIX}/lib -L${INSTALL_PREFIX}/lib64"
export CPATH="${INSTALL_PREFIX}/include"
export LD_LIBRARY_PATH="${INSTALL_PREFIX}/lib"
export PKG_CONFIG_PATH="${INSTALL_PREFIX}/lib/pkgconfig:$PKG_CONFIG_PATH"

compile_gnu_source ncurses ${NCURSES_VERSION} tar.gz
compile_gnu_source texinfo ${TEXINFO_VERSION} tar.gz
compile_gnu_source readline ${READLINE_VERSION} tar.gz
compile_gnu_source gmp ${GMP_VERSION} tar.xz
compile_gnu_source nettle ${NETTLE_VERSION} tar.gz
compile_gnu_source gnutls ${GNUTLS_VERSION} tar.xz

if [ ! -d "jansson" ]; then
    git clone https://github.com/akheron/jansson jansson
fi
cd jansson
mkdir -p build && cd build || exit 1
cmake .. -DJANSSON_BUILD_DOCS=OFF -DCMAKE_INSTALL_PREFIX=${INSTALL_PREFIX}
make -j && make install

cd "${PROGRAM_DIR}"
if [ ! -d "universal-ctags" ]; then
    git clone https://github.com/universal-ctags/ctags.git universal-ctags
fi
cd universal-ctags
./autogen.sh
./configure --prefix="${INSTALL_PREFIX}"
make -j && make install

cd "${PROGRAM_DIR}"
if [ ! -d "emacs" ]; then
    git clone --depth=1 https://github.com/emacs-mirror/emacs emacs
fi
cd emacs
./autogen.sh
./configure --prefix="${INSTALL_PREFIX}" --with-json --with-libgmp
make -j && make install
