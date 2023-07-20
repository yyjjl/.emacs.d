# -*- coding: utf-8 -*-

import os

from utils import download_github_assets, get_system_type, run_command


def filter_by_system_type(asset_info):
    system_type = get_system_type()
    name = asset_info['name']
    return name.startswith('clangd-' + system_type)


output_dir = os.path.expanduser('~/.emacs.d/.cache/lsp/clangd')

output_files = download_github_assets('clangd', 'clangd', output_dir, filter_by_system_type)

assert output_files, 'Cannot find assets'

run_command(f'unzip {output_files[0]}', directory=output_dir)
