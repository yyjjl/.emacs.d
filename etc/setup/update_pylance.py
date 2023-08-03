# -*- coding: utf-8 -*-

import io
import json
import os
import zipfile

import requests

from utils import logger

base_url = (
    'https://ms-python.gallery.vsassets.io'
    '/_apis/public/gallery/publisher/ms-python/extension/vscode-pylance/latest/assetbyname/'
    'Microsoft.VisualStudio.Services.VSIXPackage'
)

output_dir = os.path.expanduser('~/.emacs.d/.cache/lsp/pylance')

os.makedirs(output_dir, exist_ok=True)

zip_bytes = io.BytesIO()
rsp = requests.get(base_url, stream=True)
for data in rsp.iter_content(1024):
    zip_bytes.write(data)

with zipfile.ZipFile(zip_bytes, mode='r') as fp:
    package = json.loads(fp.read('extension/package.json'))

    version = package['version']
    logger.info('downloaded version %s', version)

    fp.extractall(output_dir)
