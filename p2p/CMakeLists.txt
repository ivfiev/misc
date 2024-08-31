cmake_minimum_required(VERSION 3.28)
project(p2p C)

set(CMAKE_C_STANDARD 23)

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/bin")

add_executable(p2p main.c
        p2p.h
        cb.c
        socket.c
        peer.c
        log.c
        hashtable.c
        utils.c
)

target_link_libraries(p2p m)