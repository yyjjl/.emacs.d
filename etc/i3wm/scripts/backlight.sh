#!/usr/bin/env bash

LIGHT_EXE=~/.emacs.d/.cache/light/build/bin/light

if [ "$1" = "up" ];then
    ${LIGHT_EXE} -A 5
elif [ "$1" = "down" ];then
    ${LIGHT_EXE} -U 5
fi

N=$(${LIGHT_EXE} -G)
N=${N%.*}

# if [ "$N" -lt 10 ]; then
#     icon=notification-display-brightness
# elif [ "$N" -lt 30 ]; then
#     icon=notification-display-brightness-low
# elif [ "$N" -lt 60 ]; then
#     icon=notification-display-brightness-medium
# elif [ "$N" -lt 90 ]; then
#     icon=notification-display-brightness-high
# else
#     icon=notification-display-brightness-full
# fi

notify-send "Brightness ${N}" -i "notification-display-brightness" \
            -h string:x-canonical-private-synchronous:brightness \
            -h int:value:"${N}"
