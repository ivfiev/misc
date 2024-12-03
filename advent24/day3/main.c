#include "advent.h"

size_t parse_mul(char *input, int *n, int *m) {
  char buf[32];
  char closing;
  int bytes, scanned;
  strncpy(buf, input, sizeof(buf));
  scanned = sscanf(buf, "mul(%d,%d%c%n", n, m, &closing, &bytes);
  return closing == ')' && scanned == 3 ? bytes : 0;
}

int main(int argc, char **argv) {
  char *lines[16];
  size_t count = read_lines(argv[1], lines, 16);
  uint64_t sum1 = 0, sum2 = 0;
  int enabled = 1;
  for (int i = 0; i < count; i++) {
    char *line = lines[i];
    while (*line) {
      int n, m, parsed = 0;
      uint64_t mul = 0;
      parsed = parse_mul(line, &n, &m);
      if (parsed > 0) {
        mul = n * m;
      } else if (strncmp(line, "do()", 4) == 0) {
        enabled = 1;
      } else if (strncmp(line, "don't()", 7) == 0) {
        enabled = 0;
      }
      line += parsed > 0 ? parsed : 1;
      sum1 += mul;
      sum2 += enabled * mul;
    }
  }
  printf("Part 1: [%ld]\n", sum1);
  printf("Part 2: [%ld]\n", sum2);
  free_lines(lines, count);
  return 0;
}