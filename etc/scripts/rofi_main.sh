#!/usr/bin/env bash

rofi -no-lazy-grab \
     -modi "combi,drun" \
     -combi-modi "window,drun,ssh" \
     -show-icons \
     -icon-theme Papirus \
     -kb-mode-next "Tab" \
     -kb-row-tab "" \
     -theme "Monokai" \
     -show combi
