#!/usr/bin/env bash

# Set up colours
if tty -s;then
    export RED=${RED:-$(tput setaf 1)}
    export allGREEN=${GREEN:-$(tput setaf 2)}
    export YLW=${YLW:-$(tput setaf 3)}
    export BLUE=${BLUE:-$(tput setaf 4)}
    export RESET=${RESET:-$(tput sgr0)}
else
    export RED=
    export GREEN=
    export YLW=
    export BLUE=
    export RESET=
fi

base_dir=$(dirname "$0")

# Timestamp
now () {
    date +"%H:%M:%S"
}

# Logging functions instead of echo
log () {
    echo "${BLUE}$(now)${RESET} $1"
}

info () {
    log "${GREEN}INFO${RESET}: $1"
}

warn () {
    log "${YLW}WARN${RESET}: ${1}"
}

critical () {
    log "${RED}CRIT${RESET}: ${1}"
}

# Check whether a command exists - returns 0 if it does, 1 if it does not
exists() {
    command -v "$1" >/dev/null 2>&1
    return $?
}

apt-package-installed() {
    dpkg-query -l "$1" 2>&1 | grep -i '^ii' >/dev/null 2>&1
    return $?
}

export -f now
export -f log
export -f info
export -f warn
export -f critical
export -f exists
export -f apt-package-installed

# Get command line arguments
while [[ $# -gt 0 ]]; do
    key="$1"
    shift # past key
    case $key in
        -l|--list)
            echo "Sub-scripts:"
            for script in emacs/*.sh; do
                script=${script#emacs/install-}
                echo "  ${script%.*}"
            done
            exit 0
            ;;
        -h|--help)
            echo >&2  "usage: $0 <sub-scripts> ..."
            exit 0
            ;;
        *)
            POSITIONAL+=("${key}")
            ;;
    esac
done
# restore positional parameters
set -- "${POSITIONAL[@]}"

if [ "x$*" = "x" ]; then
    critical "No script to run"
    exit 1
fi

for script in "$@"; do
    script=${base_dir}/emacs/install-${script}.sh
    info "Run ${script}"
    if bash "${script}"; then
        info "Run ${script} success"
    else
        warn "Run ${script} failed !!!"
    fi
done
