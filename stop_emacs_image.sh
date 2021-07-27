#!/usr/bin/env bash

# Note: this script need to be in the parent folder, not in tests/
# because it runs a container with $PWD as bind-mount, and relies on
# both tests/learn-ocaml-tests.el and learn-ocaml.el

# Print $1 in green
green () {
    echo -e "\e[32m$1\e[0m"
}

# Print $1 in red
red () {
    echo -e "\e[31m$1\e[0m"
}

read_eid () {
    # File containing the Emacs Container ID
    feid="$PWD/learn-ocaml-emacs.pid"
    if [ -f "$feid" ]; then
        eid=$(<"$feid")
    else
        red >&2 "Error: file '$feid' does not exist."; exit 1
    fi
}


stop_emacs () {
    green "Stopping emacs..."
    ( set -x && \
      rm -f "$feid" && \
      sudo docker stop "$eid" )
}

read_eid
stop_emacs
