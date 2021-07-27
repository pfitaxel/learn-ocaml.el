#!/usr/bin/env bash

# Note: this script need to be in the parent folder, not in tests/
# because it runs a container with $PWD as bind-mount, and relies on
# both tests/learn-ocaml-tests.el and learn-ocaml.el

# It reads the environment variable $USE_PASSWD (see below).

# These files contain some useful/hidden info for ERT tests.
confirm="$PWD/confirm.txt"
teacher="$PWD/teacher.txt"
# TODO Use them.

# This file contains the Server Container ID (gen by ./run_test_backend.sh)
fcid="$PWD/learn-ocaml-server.pid"
# This file contains the Emacs Container ID (gen by ./stop_emacs_image.sh)
feid="$PWD/learn-ocaml-emacs.pid"

# Print $1 in green
green () {
    echo -e "\\e[32m$1\\e[0m"
}

# Print $1 in red
red () {
    echo -e "\\e[31m$1\\e[0m"
}

green "Beforehand: USE_PASSWD=$USE_PASSWD"
# Default mode
: "${USE_PASSWD:=false}"
# Do "export USE_PASSWD=…" before running the script to override
green "Henceforth: USE_PASSWD=$USE_PASSWD\\n"

read_cid () {
    if [ -f "$fcid" ]; then
        cid=$(<"$fcid")
    else
        red >&2 "Error: file '$fcid' does not exist."
        exit 1
    fi
}

read_eid () {
    if [ -f "$feid" ]; then
        eid=$(<"$feid")
    else
        red >&2 "Error: file '$feid' does not exist."
        exit 1
    fi
}

assert () {
    if [ $# -ne 1 ]; then
        red "ERROR, assert expects a single arg (the code to run)"
        exit 1
    fi
    sudo docker exec -i "$eid" /bin/sh -c "
set -ex
$1
"
    ret=$?
    if [ "$ret" -ne 0 ]; then
        red "FAILURE, this shell command returned exit status $ret:
\$ sudo docker exec -i $eid /bin/sh -c '$1'\\n"
        exit $ret
    fi
}

read_cid
read_eid

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
