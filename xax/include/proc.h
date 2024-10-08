#ifndef XAX_PROC_H
#define XAX_PROC_H

#include <stdint.h>
#include <stdlib.h>

typedef struct {
  char name[128];
  uintptr_t start;
  size_t size;
} mem_desc;

typedef struct {
  char *bytes;
  uintptr_t base_addr;
  size_t size; // bytes count, arr may be larger.
} mem_block;

pid_t get_pid(char *);

size_t read_mem_desc(pid_t pid, mem_desc ds[], size_t size);

int find_mem_desc(char *key, mem_desc ds[], size_t size);

int open_mem(pid_t pid);

mem_block *read_mem(int fd, uintptr_t addr, size_t size);

size_t write_mem(int fd, uintptr_t addr, char buf[], size_t size);

int close_mem(int fd);

void free_mem(mem_block *);

#endif
