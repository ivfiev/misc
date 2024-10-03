#include <stdio.h>
#include <stdlib.h>
#include "proc.h"

int main(int argc, char **argv) {
  char *proc_name = argv[1];
  pid_t pid = get_pid(proc_name);
  printf("%s\n", proc_name);
  printf("%d\n", pid);
  return 0;
}
