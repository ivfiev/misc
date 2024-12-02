#include "advent.h"

int is_safe(int *vals, size_t size) {
  int sign = SIGN(vals[1] -  vals[0]);
  for (int i = 0; i < size - 1; i++) {
    int diff = vals[i + 1] - vals[i];
    if (SIGN(diff) != sign || abs(diff) < 1 || abs(diff) > 3) {
      return 0;
    }
  }
  return 1;
}

int main(int argc, char **argv) {
  int safe1 = 0;
  int safe2 = 0;
  int values[16];
  char *toks[16];
  char *lines[1024];
  size_t count = read_lines(argv[1], lines, 1024);
  for (int i = 0; i < count; i++) {
    size_t count_toks = strsplit(lines[i], " ", toks, 16);
    for (int j = 0; j < count_toks; j++) {
      values[j] = atoi(toks[j]);
    }
    if (is_safe(values, count_toks)) {
      safe1++;
      safe2++;
    } else {
      int test[16];
      for (int j = 0; j < count_toks; j++) {
        int left = j;
        int right = count_toks - j - 1;
        memcpy(test, values, left * sizeof(int));
        memcpy(test + left, values + left + 1, right * sizeof(int));
        if (is_safe(test, count_toks - 1)) {
          safe2++;
          break;
        }
      }
    }
  } 
  printf("Part 1: [%d]\n", safe1);
  printf("Part 2: [%d]\n", safe2);
  free_lines(lines, count);
  return 0;
}