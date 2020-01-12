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

green "Beforehand: LEARNOCAML_IMAGE=$LEARNOCAML_IMAGE"
# Default learn-ocaml image
: ${LEARNOCAML_IMAGE:=ocamlsf/learn-ocaml}
# Do "export LEARNOCAML_IMAGE=…" before running test.sh to override
green "Henceforth: LEARNOCAML_IMAGE=$LEARNOCAML_IMAGE\n"

green "Beforehand: CLIENT_IMAGE=$CLIENT_IMAGE"
# Default learn-ocaml-client image
: ${CLIENT_IMAGE:=ocamlsf/learn-ocaml-client}
# Do "export CLIENT_IMAGE=…" before running test.sh to override
green "Henceforth: CLIENT_IMAGE=$CLIENT_IMAGE\n"

green "Beforehand: CLIENT_UID=$CLIENT_UID"
# Default learn-ocaml-client UID
: ${CLIENT_UID:=1000}
# Do "export CLIENT_UID=…" before running test.sh to override
green "Henceforth: CLIENT_UID=$CLIENT_UID\n"

green "Beforehand: CLIENT_PATH=$CLIENT_PATH"
# Default learn-ocaml-client final path
: ${CLIENT_PATH:=/home/coq/.local/bin/learn-ocaml-client}
# Do "export CLIENT_PATH=…" before running test.sh to override
green "Henceforth: CLIENT_PATH=$CLIENT_PATH\n"

green "Beforehand: LEARNOCAML_VERSION=$LEARNOCAML_VERSION"
# Default learn-ocaml version
: ${LEARNOCAML_VERSION:=dev}
# Do "export LEARNOCAML_VERSION=…" before running test.sh to override
green "Henceforth: LEARNOCAML_VERSION=$LEARNOCAML_VERSION\n"

green "Beforehand: EMACS_IMAGE=$EMACS_IMAGE"
# Default emacs image
: ${EMACS_IMAGE:=opam-emacs}
# Do "export EMACS_IMAGE=…" before running test.sh to override
green "Henceforth: EMACS_IMAGE=$EMACS_IMAGE\n"

SERVER_NAME="learn-ocaml-server"
EMACS_NAME="emacs-client"

extract_client () {
    mkdir -p dist
    chmod 777 dist
    # Extract the learn-ocaml-client binary
    docker run --rm --entrypoint '' -v "$PWD/dist:/dist" \
      "$CLIENT_IMAGE:$LEARNOCAML_VERSION" /bin/sh -c \
      "set -ex && exec cp \$(which learn-ocaml-client) /dist/"
    sudo chown -Rv "$CLIENT_UID:$CLIENT_UID" dist
    sudo chmod 755 dist
}

# run a server in a docker container
run_server () {
    # Run the server in background
    docker run -d --rm --name="$SERVER_NAME" --entrypoint '' -v "$PWD:/build" \
      "$LEARNOCAML_IMAGE:$LEARNOCAML_VERSION" /bin/sh -c \
      "learn-ocaml --repo=/build/tests/repo build &&
       exec learn-ocaml --repo=/build/tests/repo serve"
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
    docker run -d -i --init --rm --name="$EMACS_NAME" -v "$PWD/dist:/dist" \
      "$EMACS_IMAGE" /bin/bash
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
    docker exec -i "$EMACS_NAME" /bin/bash -c "
set -ex
$1
"
    ret=$?
    if [ "$ret" -ne 0 ]; then
        red "FAILURE, this shell command returned exit status $ret:
\$ docker exec -i $EMACS_NAME /bin/bash -c '$1'\n"
        stop_emacs
        stop_server
        exit $ret
    fi
}

###############################################################################

run_server
extract_client
run_emacs

assert "emacs --batch --eval '(pp (+ 2 2))'"

assert "cp -f /dist/learn-ocaml-client \"$CLIENT_PATH\""

assert "learn-ocaml-client --help"

assert "
cd /build/tests
learn-ocaml-client init --server=http://localhost:8080 test test
emacs --batch -l ert -l init-tests.el -l /build/learn-ocaml.el -l learn-ocaml-tests.el -f ert-run-tests-batch-and-exit
"

stop_emacs
stop_server
