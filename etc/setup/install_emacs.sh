#!/usr/bin/env bash

current_dir="$(cd "$(dirname "${BASH_SOURCE[0]}" )" > /dev/null && pwd)"

# shellcheck source=./utils.sh
. "${current_dir}/utils.sh"

emacs_version=26.3
readline_version=6.3
ncurses_version=6.1

program_dir=${HOME}/program

if [ "$EUID" -ne 0 ]; then
    install_prefix=${HOME}/.local
else
    install_prefix=/usr/local
    apt build-dep -y emacs25
    apt install -y libwebkit2gtk-4.0-dev exuberant-ctags
fi

mkdir -p "${program_dir}/"
cd "${program_dir}/" || exit 1

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
        if ! (./configure "$@" && make && make install);
        then
            critical "Cannot compile source !!!"
            rm -rf "${dir}"
            exit 1
        fi
        cd ..
    fi
}

if [ "$EUID" -ne 0 ]; then
    compile_gnu_source ncurses ${ncurses_version} tar.gz \
                       --prefix="${install_prefix}"
    compile_gnu_source readline ${readline_version} tar.gz \
                       --prefix="${install_prefix}"

    export LDFLAGS="-L${install_prefix}/lib"
    compile_gnu_source emacs ${emacs_version} tar.xz \
                       --prefix="${install_prefix}" \
                       --without-x --with-xpm=no --with-jpeg=no \
                       --with-png=no --with-gif=no --with-tiff=no --with-gnutls=no
else
    compile_gnu_source emacs ${emacs_version} tar.xz \
                       --prefix="${install_prefix}" \
                       --with-all --with-pop --with-modules \
                       --with-xwidgets --with-gconf
fi
