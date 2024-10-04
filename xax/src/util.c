#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>

void err_fatal(char *s) {
  printf("%s\n", s);
  exit(1);
}

size_t run_cmd(char *cmd, char buf[], size_t max_len) {
  FILE *fp = popen(cmd, "r");
  if (fp == NULL) {
    err_fatal(cmd);
  }
  int fd = fileno(fp);
  size_t total = 0;
  ssize_t bytes;
  do {
    bytes = read(fd, buf + total, max_len - 1 - total);
    if (bytes < 0) {
      err_fatal(strerror(errno));
    }
    total += bytes;
  } while (bytes > 0);
  if (total > 0) {
    buf[total] = 0;
  }
  fclose(fp);
  return total;
}

size_t strsplit(char *str, const char *sep, char **toks, size_t buf_len) {
  size_t i = 0;
  char *save_ptr;
  char *tmp = strtok_r(str, sep, &save_ptr);
  for (toks[i++] = tmp; i < buf_len && tmp != NULL;) {
    tmp = strtok_r(NULL, sep, &save_ptr);
    if (tmp != NULL) {
      toks[i++] = tmp;
    }
  }
  return i;
}