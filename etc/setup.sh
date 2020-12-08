#!/bin/sh

CURRENT_DIR=$(dirname "$0")

# Variables
EMACSD=$HOME/.emacs.d
DOTFILES=$HOME/.emacs.d/etc/dotfiles
FZF=$HOME/.fzf
ZSH=$HOME/.antigen

# Get OS informatio
OS=$(uname -s)
OSREV=$(uname -r)
OSARCH=$(uname -m)

# Use colors, but only if connected to a terminal, and that terminal
# supports them.

. "${CURRENT_DIR}/setup/utils.sh"

# Check git
command -v git >/dev/null 2>&1 || {
    critical "git is not installed" >&2
    exit 1
}

# Check curl
command -v curl >/dev/null 2>&1 || {
    critical "curl is not installed" >&2
    exit 1
}

is_mac() {
    [ "$OS" = "Darwin" ]
}

is_linux() {
    [ "$OS" = "Linux" ]
}

YES=0
NO=1
promote_yn() {
    eval ${2}=$NO
    read -p "$1 [y/N]: " yn
    case $yn in
        [Yy]* )    eval ${2}=$YES;;
        [Nn]*|'' ) eval ${2}=$NO;;
        *)         eval ${2}=$NO;;
    esac
}

# Brew
if is_mac; then
    log "Install Brew Cask..."
    . setup/install_brew_cask.sh
else
    . setup/install_core.sh
fi

# Clean all configurations
clean_dotfiles() {
    [ -f $HOME/.zshrc ] && mv $HOME/.zshrc $HOME/.zshrc.bak
    [ -d $EMACSD ] && mv $EMACSD $EMACSD.bak

    rm -rf $ZSH $FZF
}

# Reset configurations
if [ -d $ZSH ] || [ -d $FZF ] || [ -d $EMACSD ]; then
    promote_yn "Do you want to reset all configurations?" "continue"
    if [ $continue -eq $YES ]; then
        clean_dotfiles
    fi
fi

ln -s $EMACSD/etc/dotfiles/zshrc $HOME/.zshrc

# Entering zsh
if command -v zsh >/dev/null 2>&1; then
    if [ "$SHELL" != "$(which zsh)" ]; then
        chsh -s $(which zsh)
        log "You need to logout and login to enable zsh as the default shell."
    fi
    env zsh
else
    critical "zsh is not installed."
    exit 1
fi
