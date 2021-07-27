#!/usr/bin/env bash

# Note: this script need to be in the parent folder, not in tests/
# because it runs a container with $PWD as bind-mount, and relies on
# both tests/learn-ocaml-tests.el and learn-ocaml.el

# Print $1 in green
green () {
    echo -e "\\e[32m$1\\e[0m"
}

# Print $1 in red
red () {
    echo -e "\\e[31m$1\\e[0m"
}

read_cid () {
    # File containing the Server Container ID
    fcid="$PWD/learn-ocaml-server.pid"
    if [ -f "$fcid" ]; then
        cid=$(<"$fcid")
    else
        red >&2 "Error: file '$fcid' does not exist."
        exit 1
    fi
}


stop_server () {
    green "Stopping server..."
    ( set -x && \
      rm -f "$fcid" && \
      sudo docker stop "$cid" )
}

read_cid
stop_server
