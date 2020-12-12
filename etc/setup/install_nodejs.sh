#!/bin/bash

OS=$(uname -s)
CURRENT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}" )" > /dev/null && pwd)"

# shellcheck source=./utils.sh
. "${CURRENT_DIR}/utils.sh"

if which node; then
    log "Nodejs is installed"
else
    version=${VERSION:-$(wget -qO- https://nodejs.org/dist/latest/ | sed -nE 's|.*>node-(.*)\.pkg</a>.*|\1|p')}

    log "Nodejs version: $version"

    if [ "$OS" = "Darwin" ]; then
        output="$HOME/Downloads/node-latest.pkg"
        curl "https://nodejs.org/dist/latest/node-${version}.pkg" > "$output"
        sudo installer -store -pkg "$output" -target "/"
    else
        output="$HOME/program/node-$version"
        symbol_link="$HOME/program/node"
        mkdir -p "$output"
        curl "https://nodejs.org/dist/latest/node-${version}-linux-x64.tar.gz" | tar zxf - -C "$output" --strip-components=1

        if [[ -d "$symbol_link" ]] || [[ -h "$symbol_link" ]]; then
            mv "$symbol_link" "${symbol_link}.backup"
        fi
        ln -s "$output" "$symbol_link"
    fi
fi
