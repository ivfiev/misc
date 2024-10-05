#include <stdio.h>
#include <stdlib.h>
#include <util.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include "proc.h"

pid_t get_pid(char *proc_name) {
  char buf[32], cmd[32];
  snprintf(cmd, sizearr(cmd), "pgrep %s", proc_name);
  run_cmd(cmd, buf, sizearr(buf));
  pid_t pid = (pid_t)strtol(buf, NULL, 10);
  return pid;
}

size_t read_mem_desc(pid_t pid, mem_desc ds[], size_t max_len) {
  char buf[16 * 1024], cmd[128];
  char *lines[64];
  snprintf(cmd, sizearr(cmd), "cat /proc/%d/maps | awk '{print $1,$6}'", pid);
  run_cmd(cmd, buf, sizearr(buf));
  size_t count = strsplit(buf, "\n", lines, sizearr(lines));
  int i;
  for (i = 0; i < count && i < max_len; i++) {
    char *words[2], *hexes[2];
    strsplit(lines[i], " ", words, sizearr(words));
    strsplit(words[0], "-", hexes, sizearr(hexes));
    strncpy(ds[i].name, words[1], sizearr(ds->name));
    size_t start = strtoull(hexes[0], NULL, 16);
    size_t end = strtoull(hexes[1], NULL, 16);
    ds[i].start = start;
    ds[i].size = end - start;
  }
  return i;
}

mem_desc *find_mem_desc(char *key, mem_desc ds[], size_t length) {
  for (int i = 0; i < length; i++) {
    if (!strncmp(key, ds[i].name, sizearr(ds->name))) {
      return &ds[i];
    }
  }
  return NULL;
}

int open_mem(pid_t pid) {
  char path[32];
  snprintf(path, sizearr(path), "/proc/%d/mem", pid);
  int fd = open(path, O_RDWR);
  return fd;
}

mem_block *read_mem(int fd, size_t addr, size_t size) {
  mem_block *mem = malloc(sizeof(mem_block));
  mem->bytes = calloc(size, sizeof(char));
  lseek(fd, (off_t)addr, SEEK_SET);
  size_t count = read_all(fd, mem->bytes, size);
  mem->base_addr = addr;
  mem->size = count;
  return mem;
}

size_t write_mem(int fd, size_t addr, char buf[], size_t size) {
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