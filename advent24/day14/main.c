#include "advent.h"

#define HEIGHT 103
#define WIDTH 101

typedef struct {
  int x, y;
  int u, v;
} robot;

int parse_robot(char *str, robot *r) {
  return sscanf(str, "p=%d,%d v=%d,%d", &r->x, &r->y, &r->u, &r->v) == 4;
}

int mod(int m, int n) {
  int r = m % n;
  return r < 0 ? n + r : r;
}

void simulate(robot *r, int secs) {
  r->x = mod(r->x + secs * r->u, WIDTH);
  r->y = mod(r->y + secs * r->v, HEIGHT);
}

int quadrant(robot *r) {
  if (IN_RANGE(0, r->x, WIDTH / 2 - 1) && IN_RANGE(0, r->y, HEIGHT / 2 - 1)) return 1;
  if (IN_RANGE(WIDTH / 2 + 1, r->x, WIDTH - 1) && IN_RANGE(0, r->y, HEIGHT / 2 - 1)) return 2;
  if (IN_RANGE(0, r->x, WIDTH / 2 - 1) && IN_RANGE(HEIGHT / 2 + 1, r->y, HEIGHT - 1)) return 3;
  if (IN_RANGE(WIDTH / 2 + 1, r->x, WIDTH - 1) && IN_RANGE(HEIGHT / 2 + 1, r->y, HEIGHT - 1)) return 4;
  return 0;
}

int max_overlap(robot *rs, size_t count) {
  char map[HEIGHT][WIDTH];
  memset(map, 0, sizeof(map));
  int max = 0;
  for (int i = 0; i < count; i++)
    map[rs[i].y][rs[i].x]++;
  for (int i = 0; i < HEIGHT; i++)
    for (int j = 0; j < WIDTH; j++)
      max = MAX(max, map[i][j]);
  return max;
}

int main(int argc, char **argv) {
  char *input[4096];
  size_t count = read_lines(argv[1], input, SIZEARR(input));
  robot robots[count];
  int quadrants[] = {0, 0, 0, 0, 0};
  uint64_t part1 = 1;
  for (int i = 0; i < count; i++) {
    parse_robot(input[i], &robots[i]);
    simulate(&robots[i], 100);
  }
  for (int i = 0; i < count; i++) {
    quadrants[quadrant(&robots[i])]++;
  }
  for (int i = 1; i <= 4; i++) {
    part1 *= quadrants[i];
  }
  printf("Part 1: [%ld]\n", part1);
  for (int secs = 101;; secs++) {
    for (int i = 0; i < count; i++) {
      simulate(&robots[i], 1);
    }
    if (max_overlap(robots, count) == 1) {
      printf("Part 2: [%d]\n", secs);
      break;
    }
  }
  free_lines(input, count);
  return 0;
}
