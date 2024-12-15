#include "advent.h"

#define MAX_HEIGHT 64
#define MAX_WIDTH 64
#define EMPTY 0
#define ROBOT 1
#define BOX 2
#define WALL 3

typedef struct {
  uint8_t robot_row, robot_col;
  uint64_t boxes[MAX_HEIGHT];
  uint64_t walls[MAX_HEIGHT];
} encoding;

int read(encoding *enc, int row, int col) {
  if (enc->robot_row == row && enc->robot_col == col) {
    return ROBOT;
  }
  if (enc->boxes[row] & (1ul << col)) {
    return BOX;
  }
  if (enc->walls[row] & (1ul << col)) {
    return WALL;
  }
  return EMPTY;
}

void toggle(encoding *enc, int type, int row, int col) {
  if (type == ROBOT) {
    enc->robot_row = row;
    enc->robot_col = col;
  } else if (type == BOX) {
    enc->boxes[row] ^= (1ul << col);
  } else if (type == WALL) {
    enc->walls[row] ^= (1ul << col);
  }
}

encoding parse(char **map, size_t height, size_t width) {
  encoding enc;
  memset(&enc, 0, sizeof(enc));
  for (int i = 0; i < height; i++) {
    for (int j = 0; j < width; j++) {
      if (map[i][j] == '@') {
        toggle(&enc, ROBOT, i, j);
      } else if (map[i][j] == 'O') {
        toggle(&enc, BOX, i, j);
      } else if (map[i][j] == '#') {
        toggle(&enc, WALL, i, j);
      }
    }
  }
  return enc;
}

int deltas(char v, int *drow, int *dcol) {
  if (v == '^') {
    *drow = -1;
    *dcol = 0;
    return 1;
  }
  if (v == '>') {
    *drow = 0;
    *dcol = 1;
    return 1;
  }
  if (v == '<') {
    *drow = 0;
    *dcol = -1;
    return 1;
  }
  if (v == 'v') {
    *drow = 1;
    *dcol = 0;
    return 1;
  }
  return 0;
}

int robot_step(encoding *enc, char v) {
  int drow, dcol, box;
  if (deltas(v, &drow, &dcol)) {
    int row = enc->robot_row + drow;
    int col = enc->robot_col + dcol;
    int type = read(enc, row, col);
    if (type == EMPTY) {
      toggle(enc, ROBOT, row, col);
      return 1;
    }
    if (type == WALL) {
      return 0;
    }
    if (type == BOX) {
      for (box = 1;; box++) {
        type = read(enc, row + box*drow, col + box*dcol);
        if (type == WALL) {
          // no need for bounds check
          return 0;
        } else if (type == EMPTY) {
          toggle(enc, BOX, row, col);
          toggle(enc, BOX, row + box*drow, col + box*dcol);
          toggle(enc, ROBOT, row, col);
          return 1;
        }
      }
    }
  }
  return 0;
}

int main(int argc, char **argv) {
  char *input[1024], *map[1024], *cmds[1024];
  size_t lines = read_lines(argv[1], input, SIZEARR(input));
  size_t height, width, count;
  uint64_t part1 = 0;
  for (height = 0; height < lines && strcmp(input[height], "\n") != 0; height++) {
    map[height] = input[height];
  }
  width = strlen(*input);
  count = lines - height - 1;
  for (int i = 0; i < count; i++) {
    cmds[i] = input[height + 1 + i];
  }
  encoding enc = parse(map, height, width);
  for (int i = 0; i < count; i++) {
    for (int c = 0; c < strlen(cmds[i]); c++) {
      robot_step(&enc, cmds[i][c]);
    }
  }
  for (int row = 0; row < height; row++) {
    for (int col = 0; col < width; col++) {
      if (read(&enc, row, col) == BOX) {
        part1 += 100 * row + col;
      }
    }
  }
  printf("Part 1: [%ld]\n", part1);
  free_lines(input, lines);
  return 0;
}