#!/bin/bash

set -xe

# pip install --user epc

cd ~/.emacs.d

mkdir -p site-lisp && cd site-lisp

if [ -d lsp-bridge ]; then
    cd lsp-bridge && git pull
else
    git clone https://github.com/manateelazycat/lsp-bridge.git lsp-bridge || cd lsp-bridge
fi


