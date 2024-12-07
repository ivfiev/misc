#include "advent.h"

#define CONCAT(x, y) (x * pow(10, (int)(log10(y) + 1)) + y)

int trve(uint64_t equation[], size_t length, uint64_t target, int flags, int ix, uint64_t acc) {
  if (ix == length) {
    return acc == target;
  }
  return 
    flags & 0x1 && trve(equation, length, target, flags, ix + 1, acc + equation[ix]) ||
    flags & 0x2 && trve(equation, length, target, flags, ix + 1, acc * equation[ix]) ||
    flags & 0x4 && trve(equation, length, target, flags, ix + 1, CONCAT(acc, equation[ix]));
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
    if (trve(equation + 2, count - 2, equation[0], 0x3, 0, equation[1])) {
      part1 += equation[0]; 
    }
    if (trve(equation + 2, count - 2, equation[0], 0x7, 0, equation[1])) {
      part2 += equation[0];
    }
  }
  printf("Part 1: [%ld]\n", part1);
  printf("Part 2: [%ld]\n", part2);
  return 0;
}
