#!/usr/bin/env bash

current_dir="$(cd "$(dirname "${BASH_SOURCE[0]}" )" > /dev/null && pwd)"

# shellcheck source=packages/color.sh
. ${current_dir}/packages/color.sh

# Check whether a command exists - returns 0 if it does, 1 if it does not
exists() {
    command -v "$1" >/dev/null 2>&1
    return $?
}

apt-package-installed() {
    dpkg-query -l "$1" 2>&1 | grep -i '^ii' >/dev/null 2>&1
    return $?
}

xargs sudo apt install -y < packages/apt-packages || exit 1
xargs cabal install < packages/cabal-packages || exit 1
xargs pip3 install < packages/python-packages || exit 1

sudo apt install nodejs -y || exit 1
sudo npm install -g n || exit 1
sudo n latest || exit 1
sudo apt remove --purge nodejs -y || exit 1

xargs sudo npm install -g < packages/npm-packages
xargs sudo cpan install < packages/cpan-packages

# zsh
sudo chsh -s "$(which zsh)"
if [ -d "${HOME}/.oh-my-zsh" ]; then
    echo "oh-my-zsh is installed"
else
    git clone https://github.com/robbyrussell/oh-my-zsh ~/.oh-my-zsh
    sh ~/.oh-my-zsh/tools/install.sh
fi

# stack
if which stack; then
    echo "stack is installed"
else
    curl -sSL https://get.haskellstack.org/ | sh
    echo -n "
templates:
  params: null
system-ghc: true
package-indices:
- name: Tsinghua
  http: http://mirrors.tuna.tsinghua.edu.cn/hackage/00-index.tar.gz
  download-prefix: http://mirrors.tuna.tsinghua.edu.cn/hackage/package/
" > ~/.stack/config.yaml
    stack update
    # stack install idris --resolver=lts-6.35
fi

# GNU Global

# cquery
mkdir -p ~/.emacs.d/var
if [ -d "${HOME}/.emacs.d//var/cquery" ]; then
    echo "cquery is installed"
else
    if cd ~/.emacs.d/var; then
        git clone https://github.com/cquery-project/cquery cquery
        if cd cquery && mkdir -p build && cd build; then
            cmake .. && make
        fi
    fi
fi
if  [ -d "${HOME}/.emacs.d/var/translate-shell" ]; then
    echo "translate-shell is installed"
else
    if cd ~/.emacs.d/var ; then
        git clone https://github.com/soimort/translate-shell  translate-shell
        make -C translate-shell
    fi
fi

# rust
curl https://sh.rustup.rs -sSf | sh
rustup update
xargs rustup component add < packages/rust-packages
cargo install ripgrep
