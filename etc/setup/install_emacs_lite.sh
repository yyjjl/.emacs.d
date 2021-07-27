#!/bin/bash

CURRENT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}" )" > /dev/null && pwd)

. ${CURRENT_DIR}/utils.sh

NETTLE_VERSION=3.4.1
GNUTLS_VERSION_MAJOR=3.6
GNUTLS_VERSION_MINOR=16
GMP_VERSION=6.2.0
TEXINFO_VERSION=6.8
READLINE_VERSION=6.3
NCURSES_VERSION=6.1
LIBUNISTRING_VERSION=0.9.10

CMAKE_URL=https://github.com/Kitware/CMake/releases/download/v3.21.0/cmake-3.21.0-linux-x86_64.sh

INSTALL_PREFIX=${HOME}/.local
PROGRAM_DIR=${HOME}/program

mkdir -p "${PROGRAM_DIR}/" && cd "${PROGRAM_DIR}/" || exit 1

download_and_compile() {
    local url=$1
    local fmt=$2
    local dir=$3
    shift;shift;shift

    local progress=$(cd $dir && get_progress)

    if [ "$progress" = "" ]; then
	info "wget $url ..."
	if [ ! -d $dir ]; then
	    if ! wget -O- "$url" | tar $fmt; then
		critical "Cannot download $dir !!!"
		rm -rf $dir
		exit 1
            fi
	fi
	progress="downloaded"
	cd $dir && set_progress $progress || exit 1
    else
	info "$dir is $progress ..."
	cd $dir || exit 1
    fi

    if [ "$progress" = "downloaded" ]; then
	if ! (./configure --prefix="${INSTALL_PREFIX}" "$@"); then
	    critical "Cannot configure $dir !!!"
	    exit 1
	fi
	progress="configured"
	set_progress $progress
    fi

    if [ "$progress" = "configured" ]; then
	if ! (make -j && make install); then
	    critical "Cannot build $dir !!!"	    
	    exit 1
	fi
	progress="built"
	set_progress $progress
    fi

    cd ..
}

download_and_compile_gnu_software() {
    local dir="$1-$2"
    local ext="$3"
    local url="https://ftp.gnu.org/gnu/$1/${dir}.${ext}"
    local fmt="-x"
    shift; shift; shift

    if [ "${ext}" = "tar.gz" ]; then
        fmt="-zx"
    elif [ "${ext}" = "tar.xz" ]; then
        fmt="-Jx"
    fi

    download_and_compile $url $fmt $dir "$@"
}

download_and_compile_gnutls() {
    local dir="gnutls-$1.$2"
    local url="https://www.gnupg.org/ftp/gcrypt/gnutls/v$1/gnutls-$1.$2.tar.xz"
    shift; shift

    download_and_compile $url "-Jx" $dir "$@"
}

download_and_compile_from_github() {
    local builder=$1
    local url=$2
    local dir=$3
    shift;shift;shift

    local current_dir=$(pwd)

    if [ ! -d $dir ]; then
	git clone $url $dir && cd $dir || exit 1
    else
	cd $dir && git pull $url || exit 1
    fi

    local progress=$(get_progress)

    if [ "$builder" = "cmake" ]; then
	mkdir -p build && cd build || exit 1
	if [ "$progress" = "" ]; then
	    cmake .. -DCMAKE_INSTALL_PREFIX=${INSTALL_PREFIX} "$@"
	fi
    else
	if [ "$progress" = "" ]; then
	    if [ -x "autogen.sh" ]; then
		./autogen.sh
	    fi
	    ./configure --prefix="${INSTALL_PREFIX}" "$@"
	fi
    fi

    if [ "$?" = 0 ]; then
	progress="configured"
	set_progress $progress
    fi

    if [ "$progress" = "configured" ]; then
	
    fi

    echo "return code: $?"

    # make -j && make install
}

export PATH="${INSTALL_PREFIX}/bin:$PATH"
export LDFLAGS="-L${INSTALL_PREFIX}/lib -L${INSTALL_PREFIX}/lib64"
export CPATH="${INSTALL_PREFIX}/include"
export LD_LIBRARY_PATH="${INSTALL_PREFIX}/lib"
export PKG_CONFIG_PATH="${INSTALL_PREFIX}/lib/pkgconfig:$PKG_CONFIG_PATH"

download_and_compile_gnu_software ncurses ${NCURSES_VERSION} tar.gz
download_and_compile_gnu_software texinfo ${TEXINFO_VERSION} tar.gz
download_and_compile_gnu_software readline ${READLINE_VERSION} tar.gz
download_and_compile_gnu_software gmp ${GMP_VERSION} tar.xz
download_and_compile_gnu_software nettle ${NETTLE_VERSION} tar.gz
download_and_compile_gnu_software libunistring ${LIBUNISTRING_VERSION} tar.gz
download_and_compile_gnutls ${GNUTLS_VERSION_MAJOR} ${GNUTLS_VERSION_MINOR}

download_and_compile_from_github cmake https://github.com/akheron/jansson jansson

# if [ ! -d "jansson" ]; then
#     git clone https://github.com/akheron/jansson jansson
# fi
# cd jansson


# cd "${PROGRAM_DIR}"
# if [ ! -d "universal-ctags" ]; then
#     git clone https://github.com/universal-ctags/ctags.git universal-ctags
# fi
# cd universal-ctags
# ./autogen.sh
# ./configure --prefix="${INSTALL_PREFIX}"
# make -j && make install

# cd "${PROGRAM_DIR}"
# if [ ! -d "emacs" ]; then
#     git clone --depth=1 https://github.com/emacs-mirror/emacs emacs    
# fi
# cd emacs
# ./autogen.sh
# ./configure --prefix="${INSTALL_PREFIX}" --with-json --with-libgmp
# make -j && make install
