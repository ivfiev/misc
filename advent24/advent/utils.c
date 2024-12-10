#include "advent.h"

static void trim_end(char *str) {
  int i = strlen(str) - 1;
  while (i > 0 && str[i] == '\n') {
    str[i--] = 0;
  }
}

size_t read_lines(const char *file, char **lines, size_t size) { 
  size_t count = 0;
  char buf[1024 * 32];
  FILE *fp = fopen(file, "r");
  if (fp == NULL) {
    puts(strerror(errno));
    exit(1);
  }
  while (count < size && fgets(buf, sizeof(buf), fp)) {
    trim_end(buf);
    lines[count++] = strdup(buf);
  }
  fclose(fp);
  return count;
}

void free_lines(char **lines, size_t size) {
  for (int i = 0; i < size; i++) {
    free(lines[i]);
  }
}

int int_cmp(const void *ptr_i, const void *ptr_j) {
  return *(int *)ptr_i - *(int *)ptr_j;
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
