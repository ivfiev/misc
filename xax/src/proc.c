#include <stdio.h>
#include <stdlib.h>
#include <util.h>
#include <string.h>
#include "proc.h"

pid_t get_pid(char *proc_name) {
  char buf[32], cmd[32];
  snprintf(cmd, sizearr(cmd), "pgrep %s", proc_name);
  run_cmd(cmd, buf, sizearr(buf));
  pid_t pid = (pid_t)strtol(buf, NULL, 10);
  return pid;
}

size_t read_mem_maps(pid_t pid, mem_map maps[], size_t max_maps) {
  char buf[16 * 1024], cmd[128];
  char *lines[64];
  snprintf(cmd, sizearr(cmd), "cat /proc/%d/maps | awk '{print $1,$6}'", pid);
  run_cmd(cmd, buf, sizearr(buf));
  size_t count = strsplit(buf, "\n", lines, sizearr(lines));
  int i;
  for (i = 0; i < count && i < max_maps; i++) {
    char *words[2], *hexes[2];
    strsplit(lines[i], " ", words, sizearr(words));
    strsplit(words[0], "-", hexes, sizearr(hexes));
    strncpy(maps[i].name, words[1], sizearr(maps->name));
    size_t start = strtoull(hexes[0], NULL, 16);
    size_t end = strtoull(hexes[1], NULL, 16);
    maps[i].start = start;
    maps[i].length = end - start;
  }
  return i;
}