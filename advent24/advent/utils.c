#include "advent.h"

size_t read_lines(const char *file, char **lines, size_t size) { 
  size_t count = 0;
  char buf[1024];
  FILE *fp = fopen(file, "r");
  if (fp == NULL) {
    puts(strerror(errno));
    exit(1);
  }
  while (fgets(buf, sizeof(buf), fp)) {
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