#!/usr/bin/env python3

import os

from utils import install_from_git

install_from_git(
    url='https://github.com/haikarainen/light',
    build_commands=[
        './autogen.sh',
        './configure --prefix=$(pwd)/build',
        'make install',
        'sudo chown root build/bin/light && sudo chmod +s build/bin/light',
    ],
    output_dir=os.path.expandvars('$HOME/.emacs.d/var')
)
