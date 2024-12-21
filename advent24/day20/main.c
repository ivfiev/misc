#include "advent.h"

#define PACK(packed, row, col) \
  kv packed = {.uint64 = 0}; \
  packed.vec16[0] = row; \
  packed.vec16[1] = col

#define UNPACK(packed, row, col) \
  short row = packed.vec16[0]; \
  short col = packed.vec16[1]

int DS[][2] = {{1, 0}, {-1, 0}, {0, 1}, {0, -1}};

int find(char **map, size_t height, size_t width, char c, int *row, int *col) {
  for (int i = 0; i < height; i++) {
    for (int j = 0; j < width; j++) {
      if (map[i][j] == c) {
        *row = i;
        *col = j;
        return 1;
      }
    }
  }
  return 0;
}

hashtable *bfs(char **map, size_t height, size_t width, int start_row, int start_col, int end_row, int end_col) {
  deque *que = deq_new();
  hashtable *black = hash_new(256, hash_int, hash_cmp_int);
  PACK(start, start_row, start_col);
  deq_push_tail(que, start);
  hash_set(black, start, KV(.uint64 = 0));
  while (deq_len(que) > 0) {
    kv node = deq_pop_head(que);
    uint64_t ps = hash_getv(black, node).uint64;
    UNPACK(node, row, col);
    if (row == end_row && col == end_col) {
      break;
    }
    for (int i = 0; i < SIZEARR(DS); i++) {
      int row_adj = row + DS[i][0];
      int col_adj = col + DS[i][1];
      PACK(adj, row_adj, col_adj);
      if (!IN_RANGE(0, row_adj, height - 1) || !IN_RANGE(0, col_adj, width - 1)) {
        continue;
      }
      if (map[row_adj][col_adj] == '#') {
        continue;
      }
      if (hash_hask(black, adj)) {
        continue;
      }
      hash_set(black, adj, KV(.uint64 = 1 + ps));
      deq_push_tail(que, adj);
    }
  }
  deq_free(que);
  return black;
}

int main(int argc, char **argv) {
  char *map[256];
  size_t height = read_lines(argv[1], map, SIZEARR(map));
  size_t width = strlen(*map);
  int start_row, start_col, end_row, end_col;
  uint64_t part1 = 0, part2 = 0;
  find(map, height, width, 'S', &start_row, &start_col);
  find(map, height, width, 'E', &end_row, &end_col);
  PACK(start, start_row, start_col);
  PACK(end, end_row, end_col);
  hashtable *no_cheating = bfs(map, height, width, start_row, start_col, end_row, end_col);
  uint64_t true_time = hash_getv(no_cheating, end).uint64;
  size_t path_len = hash_len(no_cheating);
  kv path[path_len];
  FOREACH_KV(no_cheating, { path[val.uint64] = key; });
  for (int i = 0; i < path_len; i++) {
    UNPACK(path[i], from_row, from_col);
    for (int j = i + 1; j < path_len; j++) {
      UNPACK(path[j], to_row, to_col);
      uint64_t dist = abs(from_row - to_row) + abs(from_col - to_col);
      uint64_t new_time = true_time - (j - i) + dist;
      if (true_time - 100 >= new_time) {
        part1 += dist == 2;
        part2 += dist <= 20;
      }
    }
  }
  printf("Part 1: [%ld]\n", part1);
  printf("Part 2: [%ld]\n", part2);
  hash_free(no_cheating);
  return 0;
}