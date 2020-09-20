#!/usr/bin/env bash

current_dir="$(cd "$(dirname "${BASH_SOURCE[0]}" )" > /dev/null && pwd)"

# shellcheck source=./utils.sh
. "${current_dir}/utils.sh"

mkdir -p "${HOME}/.config/pip/"

echo -n "[global]
index-url = https://pypi.tuna.tsinghua.edu.cn/simple
" >  "${HOME}/.config/pip/pip.conf"

if ! xargs pip3 install --user < "${current_dir}/packages/python-packages"; then
    critical "Fail to install python packages !!!"
    exit 1
fi
