#include <stdio.h>
#include <stdlib.h>
#include "proc.h"
#include "util.h"
#include "scan.h"

int main(int argc, char **argv) {
  mem_desc ds[128];
  char *proc_name = argv[1];
  pid_t pid = get_pid(proc_name);
  printf("%s\n", proc_name);
  printf("%d\n", pid);

  size_t dss = read_mem_desc(pid, ds, sizearr(ds));
  mem_desc *map = find_mem_desc("[stack]", ds, sizearr(ds));
  int fd = open_mem(pid);
  size_t addr = 0x7ffd3707bbcc;
  printf("%d %d\n", addr, map->start);
  write_mem(fd, addr, "ABCD", 4);
  mem_block *block = read_mem(fd, map->start, map->size);

  SCAN(block, offsets, 16, {
    ret = int32 == 0x44434241 || 123 < float32 && float32 < 124;
  });

  for (int i = 0; i < offsets_count; i++) {
    printf("%zx\n", offsets[i]);
  }

  free_mem(block);
  close_mem(fd);
  return 0;
}
