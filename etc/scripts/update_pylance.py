# -*- coding: utf-8 -*-

import io
import json
import logging
import os
# import re
import subprocess
import sys
# import tempfile
import zipfile
from pathlib import Path

# import js2py
import requests

logging.basicConfig(level=logging.INFO)

BASE_URL = (
    'https://ms-python.gallery.vsassets.io'
    '/_apis/public/gallery/publisher/ms-python/extension/vscode-pylance/latest/assetbyname/'
    'Microsoft.VisualStudio.Services.VSIXPackage'
)

OUTPUT_DIR = Path(os.path.expanduser('~/.emacs.d/.cache/lsp/pylance'))


def main():
    os.makedirs(OUTPUT_DIR, exist_ok=True)

    zip_bytes = io.BytesIO()
    rsp = requests.get(BASE_URL, stream=True)
    for data in rsp.iter_content(1024):
        zip_bytes.write(data)

    with zipfile.ZipFile(zip_bytes, mode='r') as fp:
        package = json.loads(fp.read('extension/package.json'))

        version = package['version']
        logging.info('downloaded version %s', version)

        output_dir_of_version = OUTPUT_DIR / version
        if output_dir_of_version.exists():
            logging.info('version exists .. nothing to do')
        else:
            os.makedirs(output_dir_of_version, exist_ok=True)
            fp.extractall(output_dir_of_version)

        link_file = os.path.join(OUTPUT_DIR, 'current')
        if os.path.exists(link_file):
            os.unlink(link_file)

        logging.info('symlink %s => %s', link_file, output_dir_of_version)
        os.symlink(output_dir_of_version, link_file)

    js_script = os.path.join(
        os.path.dirname(__file__),
        'hijack_pylance_server_bundle.js'
    )
    proc = subprocess.run(args=['node', js_script, version])
    sys.exit(proc.returncode)


if __name__ == '__main__':
    main()
