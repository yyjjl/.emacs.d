#!/usr/bin/env bash

current_dir="$(cd "$(dirname "${BASH_SOURCE[0]}" )" > /dev/null && pwd)"

# shellcheck source=./color.sh
. "${current_dir}/color.sh"

if ! xargs cabal install < "${current_dir}/packages/cabal-packages"; then
    critical "Fail to install cabal packages"
    exit 1
fi

sudo apt install nodejs -y || exit 1
if ! xargs sudo npm install -g < "${current_dir}/packages/npm-packages"; then
    critical "Fail to install npm packages"
    exit 1
fi

if ! xargs sudo cpan install < "${current_dir}/packages/cpan-packages"; then
    critical "Fail to install cpan packages"
    exit 1
fi

if which rustup; then
    log "Rust is installed"
else
    curl https://sh.rustup.rs -sSf | sh
    rustup update
    xargs rustup component add < "${current_dir}/packages/rust-packages"
    cargo install ripgrep
fi
