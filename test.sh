#!/bin/sh

# print in green $1
green () {
    echo -e "\e[32m$1\e[0m"
}

# print in red $1
red () {
    echo -e "\e[31m$1\e[0m"
}

# run a server in a docker container
run_server (){
    SYNC=$(pwd)/$DIR/sync
    REPO=$(pwd)/$DIR/repo

    mkdir $DIR/sync 2>/dev/null
    chmod o+w $DIR/sync

    # Run the server in background
    SERVERID=$(docker run --entrypoint '' -d \
      -v $(pwd)/$DIR:/home/learn-ocaml/actual \
      -v $SYNC:/sync -v $REPO:/repository \
      leunam217/learn-ocaml:0.11 /bin/sh \
        -c "learn-ocaml --sync-dir=/sync --repo=/repository build && 
learn-ocaml --sync-dir=/sync --repo=/repository build serve")

    # Wait for the server to be initialized
    sleep 2

    if [ "$(docker ps -q)" == "" ]; then
	red "PROBLEM, server is not running.\n"

	red "LS:"
	ls -Rl $DIR
	echo ""

	red "LOGS:"
	docker logs $SERVERID
	docker rm $SERVERID > /dev/null
	exit 1
    fi
}

DIR="test-directory" 
run_server      
echo "---> Entering $DIR:"


SUBDIR='demo'

# Test
docker exec -i $SERVERID emacs --batch --eval '(message (pp (+ 2 2)))'