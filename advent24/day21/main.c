#include "advent.h"
#define PACK(packed, row, col, index) \
  kv packed = {.uint64 = 0}; \
  packed.vec8[0] = row; \
  packed.vec8[1] = col; \
  packed.vec8[2] = index

#define UNPACK(packed, row, col, index) \
  uint8_t row = packed.vec8[0]; \
  uint8_t col = packed.vec8[1]; \
  uint8_t index = packed.vec8[2]

int DS[][2] = {{1, 0}, {-1, 0}, {0, 1}, {0, -1}};

kv bfs(hashtable *paths, char *target, char **numpad, int start_row, int start_col) {
  kv node;
  PACK(start, start_row, start_col, 0);
  deque *que = deq_new();
  hash_set(paths, start, KV(.uint64 = 0));
  deq_push_tail(que, start);
  while (deq_len(que) > 0) {
    node = deq_pop_head(que);
    UNPACK(node, row, col, index);
    if (target[index] == 0) {
      break;
    }
    if (numpad[row][col] == target[index]) {
      PACK(adj, row, col, index + 1);
      deq_push_tail(que, adj);
      hash_set(paths, adj, node);
      continue;
    }
    for (int i = 0; i < SIZEARR(DS); i++) {
      int drow = DS[i][0];
      int dcol = DS[i][1];
      PACK(adj, row + drow, col + dcol, index);
      if (numpad[row + drow][col + dcol] == '#') {
        continue;
      }
      if (hash_hask(paths, adj)) {
        continue;
      }
      deq_push_tail(que, adj);
      hash_set(paths, adj, node);
    }
  }
  deq_free(que);
  return node;
}

char *path(hashtable *paths, kv node) {
  kv prev = node;
  deque *steps = deq_new();
  for (;;) {
    UNPACK(prev, row0, col0, ix0);
    UNPACK(node, row1, col1, ix1);
    char c = 0;
    if (row1 - row0 == -1) c = 'v';
    if (row1 - row0 == 1) c = '^';
    if (col1 - col0 == -1) c = '>';
    if (col1 - col0 == 1) c = '<';
    if (ix1 < ix0) c = 'A';
    if (c) deq_push_head(steps, KV(.int32 = c));
    prev = node;
    node = hash_getv(paths, node);
    if (node.uint64 == 0) {
      break;
    }
  }
  char *str = calloc(deq_len(steps) + 1, sizeof(char));
  FOREACH_V(steps, {
    str[deq_ix] = val.int32;
  });
  deq_free(steps);
  return str;
}

int main(int argc, char **argv) {
  char *numpad[] = 
    {"#####", 
     "#789#", 
     "#456#", 
     "#123#", 
     "##0A#", 
     "#####"};
  char *arrpad[] = 
    {"#####",
     "##^A#",
     "#<v>#",
     "#####"};
  char target[1024] = "029A";
  int num = 29;
  uint64_t part1 = 0;
  for (int i = 0; i < 3; i++) {
    hashtable *paths = hash_new(128, hash_int, hash_cmp_int);
    kv end = bfs(paths, target, i == 0 ? numpad : arrpad, i == 0 ? 4 : 1, 3);
    char *str = path(paths, end);
    printf("%ld\n", strlen(str));
    part1 += i == 2 ? num * strlen(str) : 0;
    puts(str);
    strcpy(target, str);
    free(paths);
    free(str);
  }
  printf("Part 1: [%ld]\n", part1);
  // printf("%d %d %d\n", row, col, ix);
  // FOREACH_KV(paths, {
  //   UNPACK(key, to_r, to_c, to_ix);
  //   UNPACK(val, from_r, from_c, from_ix);
  //   printf("(%d %d %d) -> (%d %d %d)\n", from_r, from_c, from_ix, to_r, to_c, to_ix);
  // });
  // free(str);
  // hash_free(paths);
  return 0;
}