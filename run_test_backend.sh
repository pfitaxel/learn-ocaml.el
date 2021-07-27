#!/usr/bin/env bash

# Note: this script need to be in the parent folder, not in tests/
# because it runs a container with $PWD as bind-mount, and relies on
# both tests/learn-ocaml-tests.el and learn-ocaml.el

# It reads the environment variable $USE_PASSWD (see below).

# These files contain some useful/hidden info for ERT tests.
confirm="$PWD/confirm.txt"
teacher="$PWD/teacher.txt"

# This file contains the Server Container ID (gen by ./run_test_backend.sh)
fcid="$PWD/learn-ocaml-server.pid"

# Print $1 in green
green () {
    echo -e "\e[32m$1\e[0m"
}

# Print $1 in red
red () {
    echo -e "\e[31m$1\e[0m"
}

# Filter docker-logs
filter_confirm () {
    stdbuf -o0 grep -e "$LEARNOCAML_BASE_URL/confirm/" || :
}

green "Beforehand: LEARNOCAML_VERSION=$LEARNOCAML_VERSION"
# Default learn-ocaml version
: ${LEARNOCAML_VERSION:=oauth-moodle-dev}
# Do "export LEARNOCAML_VERSION=…" before running the script to override
green "Henceforth: LEARNOCAML_VERSION=$LEARNOCAML_VERSION\n"

green "Beforehand: LEARNOCAML_IMAGE=$LEARNOCAML_IMAGE"
# Default learn-ocaml image
: ${LEARNOCAML_IMAGE:=pfitaxel/learn-ocaml}
# Do "export LEARNOCAML_IMAGE=…" before running the script to override
green "Henceforth: LEARNOCAML_IMAGE=$LEARNOCAML_IMAGE\n"

green "Beforehand: LEARNOCAML_BASE_URL=$LEARNOCAML_BASE_URL"
# Default emacs image
: ${LEARNOCAML_BASE_URL:=http://localhost:8080}
# Do "export LEARNOCAML_BASE_URL=…" before running the script to override
green "Henceforth: LEARNOCAML_BASE_URL=$LEARNOCAML_BASE_URL\n"

green "Beforehand: USE_PASSWD=$USE_PASSWD"
# Default mode
: ${USE_PASSWD:=false}
# Do "export USE_PASSWD=…" before running the script to override
green "Henceforth: USE_PASSWD=$USE_PASSWD\n"

sudo docker pull "$LEARNOCAML_IMAGE:$LEARNOCAML_VERSION"

###############################################################################
### BACKUP OF OLD CODE ###
## Assuming /dev/fd/3 is free
#
#function trap_exit() {
#    local fi="$1"
#    exec 3>&-  # close descriptor
#    set -x; rm "$fi" || true
#}
#
#filter_confirm () { grep -e "$LEARNOCAML_BASE_URL/confirm/" || :; }
#
#fi="$PWD/logger"
#out="$PWD/confirm.txt"
#
#if [[ ! -p "$fi" ]]; then
#    mkfifo "$fi"
#else
#    echo >&2 "Info: fifo '$fi' already exists."
#fi
#
#if [[ -r "$out" ]]; then
#    echo >&2 "Info: file '$out' already exists, saving..."
#    cp --backup=t -fv "$out" "$out"
#fi
#>"$out"  # erase file "$out"
#
#cat "$fi" | filter_confirm >>"$out" &
#
#exec 3>"$fi"  # keep the input of $fi open
#
#trap 'trap_exit "$fi"' EXIT
#
## Run `cat "$fi"` or `tail --follow "$fi"` to read from the named pipe
## Run `echo ok >"$fi" &` or `echo ok >&3 &` to write in the named pipe
#
###############################################################################

wait_for_it () {
  local url="$1"
  local seconds="$2"
  shift 2  # "$@" => optional command to be run in case of success
  local elapsed=0
  green "waiting $seconds seconds for $url\\n"
  while :; do
      curl -fsS "$url" >/dev/null 2>/dev/null && break
      [ "$elapsed" -ge "$seconds" ] &&
          { red "timeout occurred after waiting $seconds seconds for $url\\n";
            return 124; }
      elapsed=$((elapsed + 1))
      sleep 1s
  done
  green "$url available after $elapsed seconds"
  if [ $# -gt 0 ]; then
      ( set -x; "$@" )
  fi
  return 0
}

gen_cid () {
    if [ -f "$fcid" ]; then
        red >&2 "Error: file '$fcid' already exists: container is running?"
        exit 1
    fi
    echo "learn-ocaml-server-$$" >"$fcid"
    cid=$(<"$fcid")
}

stop_server () {
    green "Stopping server..."
    ( set -x && \
      rm -f "$fcid" && \
      sudo docker logs "$cid"; \
      sudo docker stop "$cid" )
}

run_server () {
    local oldopt="$(set +o)"; set -x

    if [ "$USE_PASSWD" = "true" ]; then
        cp -f "$PWD/tests/use_passwd.json" "$PWD/tests/repo/server_config.json"
    else
        # TODO: Add a secret
        rm -f "$PWD/tests/repo/server_config.json"
    fi

    # Don't use the "-d" option
    sudo docker run --name="$cid" --rm -p 8080:8080 \
         -e LEARNOCAML_BASE_URL="$LEARNOCAML_BASE_URL" \
         -v "$PWD/tests/repo:/repository" \
         "$LEARNOCAML_IMAGE:$LEARNOCAML_VERSION" build serve 2>&1 | \
        filter_confirm >"$confirm" &

    set +vx; eval "$oldopt"

    # Increase this timeout if ever one sub-repo build would last > 10s
    build_timeout=10

    if ! wait_for_it "http://localhost:8080/version" "$build_timeout" sleep 1s; then
        red >&2 "PROBLEM, 'sudo docker run ... $LEARNOCAML_IMAGE:$LEARNOCAML_VERSION' does not run."
        stop_server
        exit 1
    fi
}

read_teacher () {
    echo 'Teacher token:'
    sudo docker logs "$cid" | \
        grep -e 'Initial teacher token created:' | \
        sed -e 's/^.*: //' | tr -d '[:space:]' | tee "$teacher"
    echo
}

gen_cid
run_server
read_teacher
