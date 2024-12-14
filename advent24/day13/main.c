#include "advent.h"

typedef struct {
  uint64_t x_a, y_a;
  uint64_t x_b, y_b;
  uint64_t x_p, y_p;
} claw;

claw parse_claw(char *line_a, char *line_b, char *line_p) {
  claw c;
  sscanf(line_a, "Button A: X%ld, Y%ld", &c.x_a, &c.y_a);
  sscanf(line_b, "Button B: X%ld, Y%ld", &c.x_b, &c.y_b);
  sscanf(line_p, "Prize: X=%ld, Y=%ld", &c.x_p, &c.y_p);
  return c;
}

int solve(double x0, double x1, double y0, double y1, double c0, double c1, double *a, double *b) {
  double denom = (x0 - x1 / y1 * y0);
  if (!FLOAT_EQ(denom, 0) && !isinf(denom)) {
    *a = (c0 - c1 / y1 * y0) / denom;
    *b = (c1 - *a * x1) / y1;
    return 1;
  }
  if (FLOAT_EQ(x0 / x1, y0 / y1) && FLOAT_EQ(y0 / y1, c0 / c1)) {
    return -1; // infinitely many
  }
  return 0; // none
}

int wins(claw c, uint64_t a, uint64_t b) {
  return a * c.x_a + b * c.x_b == c.x_p && a * c.y_a + b * c.y_b == c.y_p;
}

int main(int argc, char **argv) {
  char *input[4096];
  size_t count = read_lines(argv[1], input, SIZEARR(input));
  double a, b;
  uint64_t part1 = 0, part2 = 0;
  for (int i = 0; i < count; i += 4) {
    claw c = parse_claw(input[i], input[i + 1], input[i + 2]);
    if (solve(c.x_a, c.y_a, c.x_b, c.y_b, c.x_p, c.y_p, &a, &b) == 1) {
      uint64_t A = (uint64_t)round(a);
      uint64_t B = (uint64_t)round(b);
      if (wins(c, A, B)) {
        part1 += 3 * A + B;
      }
    }
    c.x_p += 10000000000000;
    c.y_p += 10000000000000;
    if (solve(c.x_a, c.y_a, c.x_b, c.y_b, + c.x_p, c.y_p, &a, &b) == 1) {
      uint64_t A = (uint64_t)round(a);
      uint64_t B = (uint64_t)round(b);
      if (wins(c, A, B)) {
        part2 += 3 * A + B;
      }
    }
  }
  printf("Part 1: [%ld]\n", part1);
  printf("Part 2: [%ld]\n", part2);
  free_lines(input, count);
  return 0;
}