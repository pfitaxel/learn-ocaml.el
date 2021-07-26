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
: ${LEARNOCAML_VERSION:=oauth-moodle-dev}
# Do "export LEARNOCAML_VERSION=…" before running test.sh to override
green "Henceforth: LEARNOCAML_VERSION=$LEARNOCAML_VERSION\n"

green "Beforehand: LEARNOCAML_IMAGE=$LEARNOCAML_IMAGE"
# Default learn-ocaml image
: ${LEARNOCAML_IMAGE:=pfitaxel/learn-ocaml}
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
    local oldopt="$(set +o)"; set -x
    # Run the server in background
    if [ "$USE_PASSWD" = "true" ]; then
        cp -f "$PWD/tests/use_passwd.json" "$PWD/tests/repo/server_config.json"
    else
        rm -f "$PWD/tests/repo/server_config.json"
    fi
    # Add -e "LEARNOCAML_BASE_URL=$LEARNOCAML_BASE_URL"?
    sudo docker run -d --rm --name="$SERVER_NAME" \
      -v "$PWD/tests/repo:/repository" \
      "$LEARNOCAML_IMAGE:$LEARNOCAML_VERSION"
    ret=$?
    set +vx; eval "$oldopt"  # has to be after "ret=$?"
    if [ "$ret" -ne 0 ]; then
        red "PROBLEM, 'sudo docker run -d ... $LEARNOCAML_IMAGE:$LEARNOCAML_VERSION' failed with exit status $ret"
        exit $ret
    fi

    # Wait for the server to be initialized
    sleep 2

    if [ -z "$(sudo docker ps -q)" ]; then
        red "PROBLEM, server is not running"
        exit 1
    fi
}

stop_server () {
    green "Stopping server..."
    ( set -x && \
      # sudo docker logs "$SERVER_NAME"
      sudo docker stop "$SERVER_NAME" )
}

# run an emacs in a docker container
run_emacs () {
    local oldopt="$(set +o)"; set -x
    # Run the server in background
    sudo docker run -d -i --init --rm --name="$EMACS_NAME" \
      -v "$PWD:/build" --network="container:$SERVER_NAME" \
      "$EMACS_IMAGE:$LEARNOCAML_VERSION"
    ret=$?
    set +vx; eval "$oldopt"  # has to be after "ret=$?"
    if [ "$ret" -ne 0 ]; then
        red "PROBLEM, 'sudo docker run -d ... $EMACS_IMAGE:$LEARNOCAML_VERSION' failed with exit status $ret"
        exit $ret
    fi
}

stop_emacs () {
    green "Stopping emacs..."
    ( set -x && \
      sudo docker stop "$EMACS_NAME" )
}

assert () {
    if [ $# -ne 1 ]; then
        red "ERROR, assert expects a single arg (the code to run)"
        exit 1
    fi
    sudo docker exec -i "$EMACS_NAME" /bin/sh -c "
set -ex
$1
"
    ret=$?
    if [ "$ret" -ne 0 ]; then
        red "FAILURE, this shell command returned exit status $ret:
\$ sudo docker exec -i $EMACS_NAME /bin/sh -c '$1'\n"
        stop_emacs
        stop_server
        exit $ret
    fi
}

###############################################################################

run_server
run_emacs

assert "emacs --version"

assert "emacs --batch --eval '(pp (+ 2 2))'"

echo

assert "learn-ocaml-client --version"

if [ "$USE_PASSWD" = "true" ]; then
    # TODO: Refactor this to run the init command from ERT's fixture
    init='learn-ocaml-client init-user -s http://localhost:8080 foo@example.com OCaml123_ Foo ""'
    selector=""
else
    init='learn-ocaml-client init --server=http://localhost:8080 Foo ""'
    selector="learn-ocaml-test-skip-use-passwd"
fi

assert "
cd /build/tests
$init
emacs --batch -l ert -l init-tests.el -l /build/learn-ocaml.el \
  -l learn-ocaml-tests.el --eval '(ert-run-tests-batch-and-exit $selector)'
"

stop_emacs
stop_server
