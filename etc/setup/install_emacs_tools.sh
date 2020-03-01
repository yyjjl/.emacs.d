#!/usr/bin/env bash

# cquery
mkdir -p ~/.emacs.d/var
if [ -d "~/.emacs.d/var/ccls" ]; then
    echo "ccls is installed"
else
    if cd ~/.emacs.d/var; then
        git clone --depth=1 --recursive https://github.com/MaskRay/ccls
        cd ccls

        wget -c http://releases.llvm.org/8.0.0/clang+llvm-8.0.0-x86_64-linux-gnu-ubuntu-18.04.tar.xz
        tar xf clang+llvm-8.0.0-x86_64-linux-gnu-ubuntu-18.04.tar.xz
        cmake -H. -BRelease \
              -DCMAKE_BUILD_TYPE=Release \
              -DCMAKE_PREFIX_PATH=$PWD/clang+llvm-8.0.0-x86_64-linux-gnu-ubuntu-18.04
        cmake --build Release
    fi
fi

if  [ -d "~/.emacs.d/var/translate-shell" ]; then
    echo "translate-shell is installed"
else
    if cd ~/.emacs.d/var ; then
        git clone --depth=1 https://github.com/soimort/translate-shell
        make -C translate-shell
    fi
fi
