#!/usr/bin/env bash

VOLUME_EXE=/usr/share/i3blocks/volume

case "$1" in
    "up")
        export BLOCK_BUTTON=4
        ;;
    "down")
        export BLOCK_BUTTON=5
        ;;
esac

N=$($VOLUME_EXE)
N=${N%\%*}
if [ "$N" = "MUTE" ]; then
    icon=audio-volume-muted-symbolic
    N=0
elif [ "$N" -lt 10 ]; then
    icon=audio-volume-low-symbolic
elif [ "$N" -lt 60 ]; then
    icon=audio-volume-medium-symbolic
else
    icon=audio-volume-high-symbolic
fi

notify-send "Volume ${N}" -i "${icon}" \
            -h string:x-canonical-private-synchronous:volume \
            -h int:value:"$N"
