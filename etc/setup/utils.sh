#!/usr/bin/env bash

PROGRESS_FILE=.install-progress

# Setup colors ################################################################
if tty -s; then
    export RED=${RED:-$(tput setaf 1)}
    export GREEN=${GREEN:-$(tput setaf 2)}
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

get_progress() {
    if [ -f $PROGRESS_FILE ]; then
	cat $PROGRESS_FILE
    fi
}

set_progress() {
    echo $1 > $PROGRESS_FILE
}

# export -f now
# export -f log
# export -f info
# export -f warn
# export -f critical
