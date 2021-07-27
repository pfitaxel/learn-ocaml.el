#!/usr/bin/env bash

# Note: this script need to be in the parent folder, not in tests/
# because it runs a container with $PWD as bind-mount, and relies on
# both tests/learn-ocaml-tests.el and learn-ocaml.el

# This file contains the Server Container ID (gen by ./run_test_backend.sh)
fcid="$PWD/learn-ocaml-server.pid"
# This file contains the Emacs Container ID (used by ./stop_emacs_image.sh)
feid="$PWD/learn-ocaml-emacs.pid"

# Print $1 in green
green () {
    echo -e "\\e[32m$1\\e[0m"
}

# Print $1 in red
red () {
    echo -e "\\e[31m$1\\e[0m"
}

green "Beforehand: LEARNOCAML_VERSION=$LEARNOCAML_VERSION"
# Default learn-ocaml version
: "${LEARNOCAML_VERSION:=oauth-moodle-dev}"
# Do "export LEARNOCAML_VERSION=…" before running the script to override
green "Henceforth: LEARNOCAML_VERSION=$LEARNOCAML_VERSION\\n"

green "Beforehand: EMACS_IMAGE=$EMACS_IMAGE"
# Default emacs image
: "${EMACS_IMAGE:=pfitaxel/emacs-learn-ocaml-client}"
# Do "export EMACS_IMAGE=…" before running the script to override
green "Henceforth: EMACS_IMAGE=$EMACS_IMAGE\\n"

sudo docker pull "$EMACS_IMAGE:$LEARNOCAML_VERSION"

gen_emacs_cid () {
    if [ -f "$feid" ]; then
        red >&2 "Error: file '$feid' already exists: container is running?"
        exit 1
    fi
    echo "learn-ocaml-emacs-$$" >"$feid"
    eid=$(<"$feid")
}

read_cid () {
    if [ -f "$fcid" ]; then
        cid=$(<"$fcid")
    else
        red >&2 "Error: file '$fcid' does not exist."
        exit 1
    fi
}

stop_emacs () {
    green "Stopping emacs..."
    ( set -x && \
      rm -f "$feid" && \
      sudo docker logs "$eid"; \
      sudo docker stop "$eid" )
}

run_emacs () {
    local oldopt; oldopt="$(set +o)"; set -x

    # Run the image in background
    sudo docker run -d -i --init --rm --name="$eid" \
      -v "$PWD:/build" --network="container:$cid" \
      "$EMACS_IMAGE:$LEARNOCAML_VERSION"
    ret=$?

    # hacky but working
    sleep 2s

    set +vx; eval "$oldopt"  # has to be after "ret=$?"

    if [ "$ret" -ne 0 ]; then
        red "PROBLEM, 'sudo docker run -d ... $EMACS_IMAGE:$LEARNOCAML_VERSION' failed with exit status $ret"
        stop_emacs
        exit $ret
    fi
}

read_cid
gen_emacs_cid
run_emacs
