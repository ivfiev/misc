cmake_minimum_required(VERSION 3.29)
project(xax C)

set(CMAKE_C_STANDARD 23)

include_directories(${CMAKE_SOURCE_DIR}/include)

add_executable(xax src/main.c
        src/proc.c
        src/util.c
        include/util.h
        include/proc.h
        src/impl/sample.c
        include/scan.h
        include/hashtable.h
        src/hashtable.c
        src/args.c
        include/args.h
        src/impl/h3sod.c
        src/impl/cli32.c
        src/scan.c
)
