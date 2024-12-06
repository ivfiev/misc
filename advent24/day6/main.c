#include "advent.h"

#define OUT_OF_BOUNDS (-1)
#define OBSTACLE 0
#define SUCCESS_STEP 1

const static int DIRECTIONS[4][2] = {{-1, 0}, {0, 1}, {1, 0}, {0, -1}};

int find_start(char **map, size_t height, size_t width, int *row, int *col, int *dir) {
  for (int i = 0; i < height; i++) {
    for (int j = 0; j < width; j++) {
      if (map[i][j] == '^') {
        *row  = i;
        *col = j;
        *dir = 0;
        return 1;
      }
    }
  }
  return 0;
}

int step(char **map, size_t height, size_t width, int *row, int *col, int dir) {
  int new_row = *row + DIRECTIONS[dir][0];
  int new_col = *col + DIRECTIONS[dir][1];
  if (!IN_RANGE(0, new_row, height - 1) || !IN_RANGE(0, new_col, width - 1)) {
    return OUT_OF_BOUNDS;
  }
  if (map[new_row][new_col] == '#') {
    return OBSTACLE;
  }
  *row = new_row;
  *col = new_col;
  return SUCCESS_STEP;
}

void turn(int *dir) {
  *dir = (*dir + 1) % SIZEARR(DIRECTIONS);
}

int run(char **map, size_t height, size_t width, int row, int col, int dir) {
  int steps = 0;
  char visited[height][width];
  memset(visited, 0, sizeof(visited));
  for (int time = 0; time - steps < height * width; time++) {
    steps += !visited[row][col];
    visited[row][col] = 1;
    int result = step(map, height, width, &row, &col, dir);
    if (result == OBSTACLE) {
      turn(&dir);
    } 
    if (result == OUT_OF_BOUNDS) {
      return steps;
    }
  }
  return -1;
}

int main(int argc, char **argv) {
  char *input[256];
  size_t height = read_lines(argv[1], input, SIZEARR(input));
  size_t width = strlen(*input) - 1;
  int row, col, dir;
  uint64_t part1 = 0, part2 = 0;
  find_start(input, height, width, &row, &col, &dir);
  part1 = run(input, height, width, row, col, dir);
  for (int i = 0; i < height; i++){
    for (int j = 0; j < width; j++) {
      if (input[i][j] == '.') {
        input[i][j] = '#';
        part2 += run(input, height, width, row, col, dir) == -1;
        input[i][j] = '.';
      }
    }
  }
  printf("Part 1: [%ld]\n", part1);
  printf("Part 2: [%ld]\n", part2);
  free_lines(input, height);
  return 0;
}