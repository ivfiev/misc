#include "advent.h"

static void search(char **map, size_t height, size_t width, deque *curr, deque *paths, int row, int col) {
#define LEGIT(i, j) (IN_RANGE(0, i, height - 1) && IN_RANGE(0, j, width - 1) && map[i][j] == c + 1)
  char c = map[row][col];  
  deq_push_tail(curr, KV(.uint64 = (uint64_t)row << 32 | col));
  if (c == '9') {
    deq_push_tail(paths, KV(.ptr = deq_copy(curr)));
    return;
  }
  if (LEGIT(row + 1, col))
    search(map, height, width, curr, paths, row + 1, col);
  if (LEGIT(row - 1, col))
    search(map, height, width, curr, paths, row - 1, col);
  if (LEGIT(row, col + 1)) 
    search(map, height, width, curr, paths, row, col + 1);
  if (LEGIT(row, col - 1))
    search(map, height, width, curr, paths, row, col - 1);
  deq_pop_tail(curr);
}

int calc_scores(deque *trails) {
  hashtable *tbl = hash_new(100, hash_int, hash_cmp_int);
  FOREACH_V(trails, {
    hash_set(tbl, KV(.uint64 = deq_at(val.ptr, 9).uint64), KV(.int32 = 1));
  });
  int score = tbl->len;
  hash_free(tbl);
  return score;
}

void free_trails(deque *trails) {
  FOREACH_V(trails, {
    deq_free(val.ptr);
  });
  deq_free(trails);
}

deque *unique_trails(char **map, size_t height, size_t width, int row, int col) {
  deque *trails = deq_new();
  deque *curr = deq_new();
  search(map, height, width, curr, trails, row, col);
  deq_free(curr);
  return trails;
}

int main(int argc, char **argv) {
  char *input[64];
  size_t height = read_lines(argv[1], input, SIZEARR(input));
  size_t width = strlen(*input);
  uint64_t part1 = 0, part2 = 0;
  for (int i = 0; i < height; i++) {
    for (int j = 0; j < width; j++) {
      if (input[i][j] == '0') {
        deque *trails = unique_trails(input, height, width, i, j);
        part1 += calc_scores(trails);
        part2 += deq_len(trails);
        free_trails(trails);
      }
    }
  }
  printf("Part 1: [%d]\n", part1);
  printf("Part 2: [%d]\n", part2);
  free_lines(input, height);
  return 0;
}