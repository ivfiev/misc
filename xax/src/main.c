#include <stdio.h>
#include <stdlib.h>
#include "proc.h"
#include "util.h"

int main(int argc, char **argv) {
  mem_map maps[128];
  char *proc_name = argv[1];
  pid_t pid = get_pid(proc_name);
  printf("%s\n", proc_name);
  printf("%d\n", pid);
  size_t count = read_mem_maps(pid, maps, sizearr(maps));
  for (int i = 0; i < count; i++) {
    printf("%s %zu %zu\n", maps[i].name, maps[i].start, maps[i].length);
  }
  return 0;
}
