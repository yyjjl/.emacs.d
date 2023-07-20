#!/bin/bash

OS=$(uname -s)
CURRENT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}" )" > /dev/null && pwd)"

. "${CURRENT_DIR}/utils.sh"

if which racket; then
    log "Racket is installed"
else
    if [ "$OS" = "Darwin" ]; then
        brew install --cask racket
    else
        sudo apt install -y racket
    fi
fi
