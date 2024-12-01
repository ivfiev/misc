#include "advent.h"

size_t read_lines(const char *file, char **lines, size_t size) { 
  size_t count = 0;
  char buf[1024];
  FILE *fp = fopen(file, "r");
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