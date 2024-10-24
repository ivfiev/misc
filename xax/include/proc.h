#ifndef XAX_PROC_H
#define XAX_PROC_H

#include "types.h"

pid_t get_pid(char *);

size_t read_mem_desc(pid_t pid, mem_desc ds[], size_t size);

size_t read_mem_blocks(pid_t pid, int mem_fd, mem_block *bs[], size_t size);

int find_mem_desc(char *key, mem_desc ds[], size_t size);

int open_mem(pid_t pid);

mem_block *read_mem_block(int fd, uintptr_t addr, size_t size);

ssize_t read_mem_bytes(int fd, uintptr_t addr, char buf[], size_t size);

union word32 read_mem_word32(int fd, uintptr_t addr);

size_t write_mem(int fd, uintptr_t addr, char buf[], size_t size);

int close_mem(int fd);

void free_mem(mem_block *);

#endif
