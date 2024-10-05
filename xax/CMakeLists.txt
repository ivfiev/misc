cmake_minimum_required(VERSION 3.29)
project(xax C)

set(CMAKE_C_STANDARD 23)

include_directories(${CMAKE_SOURCE_DIR}/include)

add_executable(xax src/main.c
        src/proc.c
        src/util.c
        include/util.h
        include/proc.h
        src/impl/train.c
        src/scan.c
        include/scan.h)
