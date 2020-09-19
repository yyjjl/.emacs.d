#!/usr/bin/env bash

if  [ -d "~/.emacs.d/var/translate-shell" ]; then
    echo "translate-shell is installed"
else
    if cd ~/.emacs.d/var ; then
        git clone --depth=1 https://github.com/soimort/translate-shell
        make -C translate-shell
    fi
fi
