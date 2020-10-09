#!/usr/bin/env bash

# backlight_get
#       Print current keyboard brightness from UPower to stdout.
backlight_get()
{
    dbus-send --type=method_call --print-reply=literal --system         \
        --dest='org.freedesktop.UPower'                                 \
        '/org/freedesktop/UPower/KbdBacklight'                          \
        'org.freedesktop.UPower.KbdBacklight.GetBrightness'             \
        | awk '{print $2}'
}

# backlight_get_max
#       Print the maximum keyboard brightness from UPower to stdout.
backlight_get_max()
{
    dbus-send --type=method_call --print-reply=literal --system       \
        --dest='org.freedesktop.UPower'                               \
        '/org/freedesktop/UPower/KbdBacklight'                        \
        'org.freedesktop.UPower.KbdBacklight.GetMaxBrightness'        \
        | awk '{print $2}'
}

# backlight_set NUMBER
#       Set the current backlight brighness to NUMBER, through UPower
backlight_set()
{
    value="$1"
    if test -z "${value}" ; then
        echo "Invalid backlight value ${value}"
    fi

    dbus-send --type=method_call --print-reply=literal --system       \
        --dest='org.freedesktop.UPower'                               \
        '/org/freedesktop/UPower/KbdBacklight'                        \
        'org.freedesktop.UPower.KbdBacklight.SetBrightness'           \
        "int32:${value}}"
}

MAX=$(backlight_get_max)
CUR=$(backlight_get)
N=$CUR

case "$1" in
    "up")
        N=$(($CUR + 1))
        ;;
    "down")
        N=$(($CUR - 1))
        ;;
    *)
        echo "$CUR"
esac

if [ "$MAX" -lt $N ] || [ $N -lt 0 ];then
    exit 1
fi

if [ ! $N -eq "$CUR" ];then
    backlight_set $N
fi

N=$(($N * 100 / $MAX))

notify-send "KbdBrightness ${N}" -i keyboard-brightness-symbolic \
            -h string:x-canonical-private-synchronous:brightness \
            -h int:value:"$N"
