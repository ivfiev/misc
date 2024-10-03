#include <stdio.h>
#include <stdlib.h>
#include <util.h>

pid_t get_pid(char *proc_name) {
  char buf[32];
  snprintf(buf, sizeof(buf), "pgrep %s", proc_name);
  FILE *f = popen(buf, "r");
  if (f == NULL) {
    err_fatal("failed to pgrep");
  }
  fgets(buf, sizeof(buf), f);
  pid_t pid = (pid_t)strtol(buf, NULL, 10);
  return pid;
}