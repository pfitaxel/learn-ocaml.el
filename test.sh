#!/bin/bash

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

green "Beforehand: LEARNOCAML_VERSION=$LEARNOCAML_VERSION"
# Default learn-ocaml version
: ${LEARNOCAML_VERSION:=dev}
# Do "export LEARNOCAML_VERSION=…" before running test.sh to override
green "Henceforth: LEARNOCAML_VERSION=$LEARNOCAML_VERSION\n"

green "Beforehand: LEARNOCAML_IMAGE=$LEARNOCAML_IMAGE"
# Default learn-ocaml image
: ${LEARNOCAML_IMAGE:=ocamlsf/learn-ocaml}
# Do "export LEARNOCAML_IMAGE=…" before running test.sh to override
green "Henceforth: LEARNOCAML_IMAGE=$LEARNOCAML_IMAGE\n"

green "Beforehand: EMACS_IMAGE=$EMACS_IMAGE"
# Default emacs image
: ${EMACS_IMAGE:=pfitaxel/emacs-learn-ocaml-client}
# Do "export EMACS_IMAGE=…" before running test.sh to override
green "Henceforth: EMACS_IMAGE=$EMACS_IMAGE\n"

SERVER_NAME="learn-ocaml-server"
EMACS_NAME="emacs-client"

# run a server in a docker container
run_server () {
    # Run the server in background
    docker run -d --rm --name="$SERVER_NAME" \
      -v "$PWD/tests/repo:/repository" \
      "$LEARNOCAML_IMAGE:$LEARNOCAML_VERSION"
    ret=$?
    if [ "$ret" -ne 0 ]; then
        red "PROBLEM, 'docker run -d ...' failed with exit status $ret"
        exit $ret
    fi

    # Wait for the server to be initialized
    sleep 2

    if [ -z "$(docker ps -q)" ]; then
        red "PROBLEM, server is not running"
        exit 1
    fi
}

stop_server () {
    green "Stopping server..."
    ( set -x && \
      # docker logs "$SERVER_NAME"
      docker stop "$SERVER_NAME" )
}

# run an emacs in a docker container
run_emacs () {
    # Run the server in background
    docker run -d -i --init --rm --name="$EMACS_NAME" \
      -v "$PWD:/build" --network="container:$SERVER_NAME" \
      "$EMACS_IMAGE:$LEARNOCAML_VERSION" /bin/sh
    ret=$?
    if [ "$ret" -ne 0 ]; then
        red "PROBLEM, 'docker run -d ...' failed with exit status $ret"
        exit $ret
    fi
}

stop_emacs () {
    green "Stopping emacs..."
    ( set -x && \
      docker stop "$EMACS_NAME" )
}

assert () {
    if [ $# -ne 1 ]; then
        red "ERROR, assert expects a single arg (the code to run)"
        exit 1
    fi
    docker exec -i "$EMACS_NAME" /bin/sh -c "
set -ex
$1
"
    ret=$?
    if [ "$ret" -ne 0 ]; then
        red "FAILURE, this shell command returned exit status $ret:
\$ docker exec -i $EMACS_NAME /bin/sh -c '$1'\n"
        stop_emacs
        stop_server
        exit $ret
    fi
}

###############################################################################

run_server
run_emacs

assert "emacs --batch --eval '(pp (+ 2 2))'"

assert "learn-ocaml-client --help"

assert "
cd /build/tests
learn-ocaml-client init --server=http://localhost:8080 test test
emacs --batch -l ert -l init-tests.el -l /build/learn-ocaml.el -l learn-ocaml-tests.el -f ert-run-tests-batch-and-exit
"

stop_emacs
stop_server
