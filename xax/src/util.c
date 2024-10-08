#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>

void err_fatal(char *s) {
  printf("%s\n", s);
  exit(1);
}

size_t read_all(int fd, char buf[], size_t size) {
  size_t total = 0;
  ssize_t bytes;
  do {
    bytes = read(fd, buf + total, size - total);
    if (bytes < 0) {
      err_fatal(strerror(errno));
    }
    total += bytes;
  } while (bytes > 0);
  return total;
}

size_t run_cmd(char *cmd, char buf[], size_t size) {
  FILE *fp = popen(cmd, "r");
  if (fp == NULL) {
    err_fatal(cmd);
  }
  int fd = fileno(fp);
  size_t total = read_all(fd, buf, size - 1);
  if (total > 0) {
    buf[total] = 0;
  }
  fclose(fp);
  return total;
}

size_t strsplit(char *str, const char *sep, char **toks, size_t size) {
  size_t i = 0;
  char *save_ptr;
  char *tmp = strtok_r(str, sep, &save_ptr);
  for (toks[i++] = tmp; i < size && tmp != NULL;) {
    tmp = strtok_r(NULL, sep, &save_ptr);
    if (tmp != NULL) {
      toks[i++] = tmp;
    }
  }
  return i;
}
