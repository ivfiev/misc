cmake_minimum_required(VERSION 3.28)
project(p2p C)

set(CMAKE_C_STANDARD 23)

add_executable(p2p main.c
        p2p.h
        cb.c
        socket.c
        error.c
        peer.c
        listener.c
        log.c
        timer.c
        hashtable.c
)
