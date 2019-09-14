#!/bin/bash

# Install `gperf` and `libtgvoip-dev`
sudo apt install -y gperf libtgvoip-dev

cd ~/.emacs.d/var || exit 1
git clone https://github.com/tdlib/td.git || exit 1
mkdir -p td/build && cd td/build || exit 1
cmake .. -DCMAKE_BUILD_PREFIX=$(pwd)/../../td-build || exit 1
make -j4 && make install || exit 1
