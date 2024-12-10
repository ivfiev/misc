#include "advent.h"

int main(int argc, char **argv) {
  char *map[128];
  size_t height = read_lines(argv[1], map, SIZEARR(map));
  size_t width = strlen(*map);
  char antinodes[height][width][2];
  deque *antennas[128];
  int part1 = 0, part2 = 0;
  memset(antinodes, 0, sizeof(antinodes));
  for (int i = 0; i < SIZEARR(antennas); i++) {
    antennas[i] = deq_new();
  }
  for (int i = 0; i < height; i++) {
    for (int j = 0; j < width; j++) {
      if (map[i][j] != '.') {
        deq_push_tail(antennas[map[i][j]], KV(.uint64 = (uint64_t)i << 32 | j));
      }
    }
  }
  for (int c = '0'; c <= 'z'; c++) {
    deque *ants = antennas[c];
    FOREACH_V(ants, {
      int row1 = val.uint64 >> 32;
      int col1 = val.uint64 & 0xFFFFFFFF;
      FOREACH_V(ants, {
        int row2 = val.uint64 >> 32;
        int col2 = val.uint64 & 0xFFFFFFFF;
        if (row1 == row2 && col1 == col2) continue;
        int dr = row1 - row2;
        int dc = col1 - col2;
        for (int k = 0; ; k++) {
          int anti_row = row1 + k * dr;
          int anti_col = col1 + k * dc;
          if (!IN_RANGE(0, anti_row, height - 1) || !IN_RANGE(0, anti_col, width - 1)) {
            break;
          }
          if (k == 1) {
            part1 += antinodes[anti_row][anti_col][0] != '#';
            antinodes[anti_row][anti_col][0] = '#';
          }
          part2 += antinodes[anti_row][anti_col][1] != '#';
          antinodes[anti_row][anti_col][1] = '#';
        }
      });
    });
  }
  printf("Part 1: [%d]\n", part1);
  printf("Part 2: [%d]\n", part2);
  free_lines(map, height);
  for (int i = 0; i < SIZEARR(antennas); i++) {
    deq_free(antennas[i]);
  }
  return 0;
}