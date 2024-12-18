#include "advent.h"

#define POS(pos, row, col) \
  kv pos; \
  memset(pos.vec16, 0, 8); \
  pos.vec16[0] = row; \
  pos.vec16[1] = col;
#define UNPOS(pos, row, col) \
  short row = pos.vec16[0]; \
  short col = pos.vec16[1]
#define HEIGHT 71
#define WIDTH 71

int DS[4][2] = {{1, 0}, {0, 1}, {-1, 0}, {0, -1}};

hashtable *bfs(hashtable *red) {
  hashtable *black = hash_new(128, hash_int, hash_cmp_int);
  deque *que = deq_new();
  POS(start, 0, 0);
  deq_push_tail(que, start);
  hash_set(black, start, KV(.uint64 = 0));
  while (deq_len(que) > 0) {
    kv pos = deq_pop_head(que);
    uint64_t steps = hash_getv(black, pos).uint64;
    UNPOS(pos, row, col);
    for (int i = 0; i < SIZEARR(DS); i++) {
      POS(adj, row + DS[i][0], col + DS[i][1]);
      if (!hash_hask(black, adj) && !hash_hask(red, adj)
          && IN_RANGE(0, adj.vec16[0], HEIGHT - 1) 
          && IN_RANGE(0, adj.vec16[1], WIDTH - 1)) {
        hash_set(black, adj, KV(.uint64 = steps + 1));
        deq_push_tail(que, adj);
      }
    }
  }
  deq_free(que);
  return black;
}

int main(int argc, char **argv) {
  char *input[4096];
  size_t count = read_lines(argv[1], input, SIZEARR(input));
  uint8_t bytes[count][2];
  hashtable *red = hash_new(128, hash_int, hash_cmp_int);
  for (int i = 0; i < count; i++) {
    PARSE_LINE(input[i], ",", 2, {
      bytes[i][token_ix] = atoi(token);
    });
  }

  // part 1
  for (int i = 0; i < 1024; i++) {
    POS(corrupted, bytes[i][0], bytes[i][1]);
    hash_set(red, corrupted, KV(.int32 = 1));
  }
  hashtable *black = bfs(red);
  POS(end, HEIGHT - 1, WIDTH - 1);
  printf("Part 1: [%ld]\n", hash_getv(black, end).uint64);
  free(black);

  // part 2
  for (int i = 1024; i < count; i++) {
    POS(corrupted, bytes[i][0], bytes[i][1]);
    hash_set(red, corrupted, KV(.int32 = 1));
    black = bfs(red);
    if (hash_getv(black, end).uint64 == 0) {
      printf("Part 2: [%d,%d]\n", bytes[i][0], bytes[i][1]);
      break;
    }
    free(black);
  }

  hash_free(red);
  hash_free(black);
  free_lines(input, count);
  return 0;
}