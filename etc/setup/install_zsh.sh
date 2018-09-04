#!/usr/bin/env bash

current_dir="$(cd "$(dirname "${BASH_SOURCE[0]}" )" > /dev/null && pwd)"

# shellcheck source=./color.sh
. "${current_dir}/color.sh"

if [ -d "${HOME}/.oh-my-zsh" ]; then
    info "oh-my-zsh is installed"
else
    git clone https://github.com/robbyrussell/oh-my-zsh ~/.oh-my-zsh
    sh ~/.oh-my-zsh/tools/install.sh
fi

if [ "$EUID" -ne 0 ]; then
    echo -n "
if [ \"\$TERM\" != \"tramp\" ] ; then
    export SHELL=/bin/zsh
    [ -z \"\$ZSH_VERSION\" ] && exec /bin/zsh -l
fi
" >> "${HOME}/.profile"
else
    chsh -s "$(which zsh)"
fi

echo -n "
export PATH=\"\${HOME}/.local/bin:\$PATH\"
export LD_LIBRARY_PATH=\"\${HOME}/.local/lib:\$LD_LIBRARY_PATH\"
export CPATH=\"\${HOME}/.local/include:\$CPATH\"

alias ec='emacsclient -c -a \"\"'
alias ff='emacsclient -c -a \"\"'

if [ -n \"\$INSIDE_EMACS\" ];then
    alias ff='emacsclient -n'
    chpwd() {
        printf '\\\\033AnSiTu %s\\n' \"\$USER\"
        print -P '\\\\033AnSiTc %d'
    }
fi
" >> "${HOME}/.zshrc"
