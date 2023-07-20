#!/usr/bin/env python3

import os

from utils import install_from_git

install_from_git(
    url='https://github.com/MaskRay/ccls',
    build_commands=[
        '''cmake -H. -Bbuild -DCMAKE_BUILD_TYPE=Release \\
                  -DCMAKE_PREFIX_PATH=/usr/lib/llvm-10 \\
                  -DLLVM_INCLUDE_DIR=/usr/lib/llvm-10/include \\
                  -DLLVM_BUILD_INCLUDE_DIR=/usr/include/llvm-10/''',
        'cmake --build build'
    ],
    output_dir=os.path.expandvars('$HOME/.emacs.d/.cache')
)
