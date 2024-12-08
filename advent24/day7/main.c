#include "advent.h"

#define CONCAT(x, y) ((x) * pow(10, (int)(log10(y) + 1)) + (y))

int trve(uint64_t acc, uint64_t equation[], size_t length, uint64_t target, int flags) {
  if (length == 0 || acc > target) {
    return acc == target;
  }
  return 
    flags & 0x1 && trve(acc + *equation, equation + 1, length - 1, target, flags) ||
    flags & 0x2 && trve(acc * *equation, equation + 1, length - 1, target, flags) ||
    flags & 0x4 && trve(CONCAT(acc, *equation), equation + 1, length - 1, target, flags);
}

int main(int argc, char **argv) {
  char *lines[1024];
  size_t equations = read_lines(argv[1], lines, SIZEARR(lines));
  uint64_t part1 = 0, part2 = 0;
  for (int i = 0; i < equations; i++) {
    uint64_t equation[32];
    PARSE_LINE(lines[i], " ", SIZEARR(equation), { 
      equation[token_ix] = atoll(token);
    });
    if (trve(equation[1], equation + 2, count - 2, equation[0], 0x3)) {
      part1 += equation[0]; 
    }
    if (trve(equation[1], equation + 2, count - 2, equation[0], 0x7)) {
      part2 += equation[0];
    }
  }
  printf("Part 1: [%ld]\n", part1);
  printf("Part 2: [%ld]\n", part2);
  free_lines(lines, equations);
  return 0;
}
