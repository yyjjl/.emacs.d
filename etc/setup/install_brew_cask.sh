#!/bin/bash

CURRENT_DIR=$(dirname "$0")

# Cask applications
CORE_APPS=(
    cmake
    ctags
    coreutils
    ripgrep
    p7zip
    fzf
    fd
    pipenv
)

CASK_APPS=(
    iterm2

    osxfuse
    google-chrome

    v2rayx # shadowsocksx-ng

    # fliqlo        # Screen Saver
    keycastr        # Show keys on the screen

    # Utilities
    adobe-acrobat-reader
)

. ${CURRENT_DIR}/utils.sh

function check {
    # Check OS
    OS=`uname -s`
    if [[ "$OS" != "Darwin" ]]; then
        critical "only install software via brew_cask on macOS" >&2
        exit 1
    fi

    # Check brew
    if ! command -v brew >/dev/null 2>&1; then
        info ">> Installing Homebrew and Cask..."

        xcode-select --install

        /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"
    fi
}

function main {
    check

    brew tap d12frosted/emacs-plus
    brew install emacs-plus@27 \
         --with-modern-black-variant-icon \
         --with-ctags \
         --with-no-frame-refocus \
         --with-xwidgets

    for app in ${CORE_APPS[@]}; do
        info ">> Installing ${app}..."
        brew install ${app}
    done

    for app in ${CASK_APPS[@]}; do
        info ">> Installing ${app}..."
        brew cask install ${app}
    done

    brew cleanup
}

main
