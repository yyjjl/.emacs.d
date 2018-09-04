#!/usr/bin/env bash

current_dir="$(cd "$(dirname "${BASH_SOURCE[0]}" )" > /dev/null && pwd)"

# shellcheck source=./color.sh
. "${current_dir}/color.sh"

if [ "$EUID" -ne 0 ]; then
    critical "This script should be run as root !!!"
    exit 1
else
    # apt-key add "${current_dir}/packages/trusted-keys"
    while IFS='' read -r line || [[ -n "$line" ]]; do
        if ! apt-add-repository -y "${line}"; then
            critical "Fail to add repository ${line}"
            exit 1
        fi
    done < "${current_dir}/packages/sources.list"

    apt update

    if ! xargs apt install -y < "${current_dir}/packages/core-apt-packages"; then
        critical "Fail to install core apt packages !!!"
    fi
fi
