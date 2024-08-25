#!/bin/bash

aliases() {
    alias up='./run.sh -u'
    alias upn='./run.sh -u -n'
    alias dn='./run.sh -d'
}

start_p2p() {
    arg="peers,nc"
    for i in {0..9}
    do
        port=$((8080 + i))
        ./p2p "$port" &  # --logs 1065 &
        echo "Started p2p instance on port $port"
        arg="$arg,$port"
    done
    if [ "$1" == "-n" ]; then
        echo "$arg"
        echo "$arg" | nc localhost 8080
    fi
}

kill_p2p() {
    pkill p2p
    echo "Killed all p2p instances"
}

if [ "$1" != "-a" ] && [ -d "cmake-build-debug" ]; then
    cd cmake-build-debug || exit
    echo "Changed directory to cmake-build-debug"
fi

if [ "$1" == "-a" ]; then 
    aliases
elif [ "$1" == "-u" ]; then
    start_p2p $2
elif [ "$1" == "-d" ]; then
    kill_p2p
else
    echo "Usage: $0 -u to start p2p instances, -d to kill them, -n for netcat, '. ./run.sh -a' for setting up shortcuts"
fi
