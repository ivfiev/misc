#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include "proc.h"
#include "util.h"

pid_t get_pid(char *proc_name) {
  char buf[32], cmd[32];
  snprintf(cmd, SIZEARR(cmd), "pgrep %s", proc_name);
  run_cmd(cmd, buf, SIZEARR(buf));
  pid_t pid = (pid_t)strtol(buf, NULL, 10);
  return pid;
}

size_t read_mem_desc(pid_t pid, mem_desc ds[], size_t size) {
  char buf[256 * size], cmd[128];
  char *lines[size];
  snprintf(cmd, SIZEARR(cmd), "cat /proc/%d/maps | awk '{print $1,$6}'", pid);
  run_cmd(cmd, buf, SIZEARR(buf));
  size_t count = strsplit(buf, "\n", lines, SIZEARR(lines));
  int i;
  for (i = 0; i < MIN(count, size); i++) {
    char *words[2], *hexes[2];
    strsplit(lines[i], " ", words, SIZEARR(words));
    strsplit(words[0], "-", hexes, SIZEARR(hexes));
    if (words[1] != NULL) {
      strncpy(ds[i].name, words[1], SIZEARR(ds->name));
    } else {
      strncpy(ds[i].name, "NULL", SIZEARR(ds->name));
    }
    uintptr_t start = strtoull(hexes[0], NULL, 16);
    uintptr_t end = strtoull(hexes[1], NULL, 16);
    ds[i].start = start;
    ds[i].size = end - start;
  }
  return i;
}

int find_mem_desc(char *key, mem_desc ds[], size_t size) {
  for (int i = 0; i < size; i++) {
    if (strcasestr(ds[i].name, key)) {
      return i;
    }
  }
  return -1;
}

int open_mem(pid_t pid) {
  char path[32];
  snprintf(path, SIZEARR(path), "/proc/%d/mem", pid);
  int fd = open(path, O_RDWR);
  return fd;
}

mem_block *read_mem(int fd, uintptr_t addr, size_t size) {
  mem_block *mem = malloc(sizeof(mem_block));
  mem->bytes = calloc(size, sizeof(char));
  lseek(fd, (off_t)addr, SEEK_SET);
  size_t count = read_all(fd, mem->bytes, size);
  mem->base_addr = addr;
  mem->size = count;
  return mem;
}

size_t write_mem(int fd, uintptr_t addr, char buf[], size_t size) {
  lseek(fd, (off_t)addr, SEEK_SET);
  return write(fd, buf, size);
}

int close_mem(int fd) {
  return close(fd);
}

void free_mem(mem_block *mem) {
  free(mem->bytes);
  free(mem);
}