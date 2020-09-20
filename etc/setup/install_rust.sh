#!/bin/bash

current_dir="$(cd "$(dirname "${BASH_SOURCE[0]}" )" > /dev/null && pwd)"

# shellcheck source=./utils.sh
. "${current_dir}/utils.sh"

if which rustup; then
    log "Rust is installed"
else
    curl https://sh.rustup.rs -sSf | sh
    rustup update
    xargs rustup component add < "${current_dir}/packages/rust-packages"
    cargo install ripgrep
fi
