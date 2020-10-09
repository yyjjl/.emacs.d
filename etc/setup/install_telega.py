#!/usr/bin/env python3

import os

from utils import install_from_git, run_command

# Install `gperf` and `libtgvoip-dev`
run_command('sudo apt install -y gperf libtgvoip-dev')

install_from_git(
    url='https://github.com/tdlib/td.git',
    build_commands=[
        'mkdir -p build',
        'cd build && cmake .. && make -j4'
    ],
    output_dir=os.path.expandvars('$HOME/.emacs.d/var')
)
