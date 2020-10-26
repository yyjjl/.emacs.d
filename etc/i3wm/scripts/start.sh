#!/usr/bin/env bash

export XDG_DATA_HOME=~/.local/share
export LC_CTYPE=zh_CN.UTF-8
export XMODIFIERS='@im=fcitx'
export XIM=fcitx
export XIM_PROGRAM=fcitx

if [ -d "$HOME/.local/bin" ]; then
    export PATH="$HOME/.local/bin:$PATH"
fi

fcitx &

xsetroot -solid "#2d2d2d"
xset r rate 220 30

emacs --daemon=i3wm -q
emacsclient -s i3wm -e "(load \"~/.emacs.d/i3wm.el\")" &

/usr/bin/i3
