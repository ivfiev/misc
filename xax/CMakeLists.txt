cmake_minimum_required(VERSION 3.29)
project(xax C)

set(CMAKE_C_STANDARD 23)

include_directories(${CMAKE_SOURCE_DIR}/include)

add_executable(xax src/main.c
        src/proc.c
        src/util.c
        include/util.h
        include/proc.h
        include/scan.h
        include/hashtable.h
        src/hashtable.c
        src/args.c
        include/args.h
        src/h3sod/h3sod.c
        src/scan.c
        include/impl.h
        src/csrh/csrh.c
        src/cli/mem_sampler.c
        src/cli/mem_analysis.c
        src/cli/mem.h
        src/cli/mem_format.c
        include/types.h
        src/cli/mem_ptr_scan.c
        src/csrh/ctl_scan.c
)

target_link_libraries(xax m)