#!/usr/bin/env python3

import os

from utils import install_from_git, try_link_file

install_from_git(
    url='https://github.com/i3-gnome/i3-gnome',
    build_commands=[
        'sed -i "s:^i3$:${HOME}/.emacs.d/etc/i3wm/scripts/start.sh:" session/i3-gnome',
        'cat session/i3-gnome',
        'sudo make install',
    ],
    output_dir=os.path.expandvars('$HOME/.emacs.d/.cache')
)

try_link_file(
    os.path.expandvars('${HOME}/.emacs.d/etc/i3wm/i3.conf'),
    os.path.expandvars('${HOME}/.config/i3/config')
)
