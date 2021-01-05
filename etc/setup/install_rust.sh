#!/bin/bash

CURRENT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}" )" > /dev/null && pwd)"

# shellcheck source=./utils.sh
. "${CURRENT_DIR}/utils.sh"

if which rustup; then
    log "Rust is installed"
else
    curl https://sh.rustup.rs -sSf | sh
    rustup update
    xargs rustup component add < "${CURRENT_DIR}/packages/rust-packages"
    cargo install ripgrep --features 'pcre2'
fi
