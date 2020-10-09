#!/usr/bin/python3

import subprocess
import re
from i3lib import get_windows, focus_window


def jump_window(n, key='focused'):
    window_list = get_windows(key)
    for i in range(len(window_list)):
        if window_list[i]["focused"]:
            next_index = (i + n) % len(window_list)
            break
    focus_window(window_list[next_index])


def jump_or_open_app(class_, *args):
    if not args:
        args = [class_]

    def matcher(x):
        return re.search(class_,
                         x['window_properties']['class'].lower())
    window_list = get_windows(None)
    try:
        app = next(filter(matcher, window_list))
    except Exception:
        app = None
    if app:
        focus_window(app)
    else:
        subprocess.Popen(['i3-msg', 'exec --no-startup-id ' + ' '.join(args)],
                         stdout=subprocess.PIPE)


if __name__ == '__main__':
    import sys
    try:
        if sys.argv[1] == 'jump':
            if len(sys.argv) == 4:
                key = sys.argv[3]
            else:
                key = None
            jump_window(int(sys.argv[2]), key)
        elif sys.argv[1] == 'open':
            jump_or_open_app(*sys.argv[2:])
    except Exception:
        pass
