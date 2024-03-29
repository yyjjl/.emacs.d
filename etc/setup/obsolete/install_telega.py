#!/usr/bin/env python3

import os

from utils import install_from_git, run_command

# Install `gperf` and `libtgvoip-dev`
run_command('sudo apt install -y gperf libtgvoip-dev libssl-dev')

output_dir=os.path.expandvars('$HOME/.emacs.d/.cache')
install_from_git(
    url='https://github.com/tdlib/td.git',
    build_commands=[
        'mkdir -p build',
        f'cd build && cmake .. -DCMAKE_INSTALL_PREFIX={output_dir}/root && make -j4 && make install'
    ],
    output_dir=output_dir
)
