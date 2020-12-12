#!/usr/bin/env bash

current_dir="$(cd "$(dirname "${BASH_SOURCE[0]}" )" > /dev/null && pwd)"

# shellcheck source=./utils.sh
. "${current_dir}/utils.sh"

if ! xargs sudo cpan install < "${current_dir}/packages/cpan-packages"; then
    critical "Fail to install cpan packages"
    exit 1
fi
