#!/bin/bash

# Function to start X instances of p2p
start_p2p() {
    for i in {0..9}
    do
        port=$((8080 + i))
        ./p2p "$port" &  # --logs 1065 &
        echo "Started p2p instance on port $port"
    done
}

# Function to kill all running instances of p2p
kill_p2p() {
    pkill p2p
    echo "Killed all p2p instances"
}

# Change directory to 'cmake-build' if it exists
if [ -d "cmake-build-debug" ]; then
    cd cmake-build-debug || exit
    echo "Changed directory to cmake-build-debug"
fi

# Check the command-line arguments
if [ "$1" == "-u" ]; then
    start_p2p
elif [ "$1" == "-d" ]; then
    kill_p2p
else
    echo "Usage: $0 -u to start p2p instances, -d to kill them"
fi
