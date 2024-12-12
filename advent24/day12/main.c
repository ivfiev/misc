#include "advent.h"

#define ROW(pos) (pos.uint64 >> 32)
#define COL(pos) (pos.uint64 & 0xFFFFFFFF)
#define POS(row, col) (KV(.uint64 = (uint64_t)(row) << 32 | (col)))
#define UNPOS(pos, row, col) \
  int row = ROW(pos); \
  int col = COL(pos)
#define IN_BOUNDS(pos, height, width) (IN_RANGE(0, ROW(pos), height - 1) && IN_RANGE(0, COL(pos), width - 1))
#define MAP(map, pos) map[ROW(pos)][COL(pos)]
#define ADJACENT(pos) {POS(ROW(pos) + 1, COL(pos)), POS(ROW(pos) - 1, COL(pos)), POS(ROW(pos), COL(pos) + 1), POS(ROW(pos), COL(pos) - 1)}

hashtable *bfs(char **map, size_t height, size_t width, int row, int col) {
  deque *que = deq_new();
  hashtable *visited = hash_new(64, hash_int, hash_cmp_int);
  deq_push_tail(que, POS(row, col));
  hash_set(visited, POS(row, col), POS(-1, -1));
  while (deq_len(que) > 0) {
    kv pos = deq_pop_head(que);
    kv adj[] = ADJACENT(pos);
    for (int i = 0; i < SIZEARR(adj); i++) {
      if (IN_BOUNDS(adj[i], height, width) && 
          !hash_hask(visited, adj[i]) && 
          MAP(map, adj[i]) == MAP(map, pos)) {
        deq_push_tail(que, adj[i]);
        hash_set(visited, adj[i], pos);
      }
    }
  }
  deq_free(que);
  return visited;
}

deque *find_plots(char **map, size_t height, size_t width) {
  deque *plots = deq_new();
  hashtable *visited = hash_new(height * width, hash_int, hash_cmp_int);
  for (int row = 0; row < height; row++) {
    for (int col = 0; col < width; col++) {
      if (!hash_hask(visited, POS(row, col))) {
        hashtable *plot = bfs(map, height, width, row, col);
        FOREACH_KV(plot, { hash_set(visited, key, val); });
        deq_push_tail(plots, KV(.ptr = plot));
      }
    }
  }
  hash_free(visited);
  return plots;
}

uint64_t calc_perimeter(hashtable *plot, char **map, size_t height, size_t width) {
  uint64_t perimeter = 0;
  FOREACH_KV(plot, {
    kv adj[] = ADJACENT(key);
    for (int i = 0; i < SIZEARR(adj); i++) {
      if (!hash_hask(plot, adj[i])) {
        perimeter++; 
      }
    }
  });
  return perimeter;
}

size_t calc_edges(hashtable *plot, char **map, size_t height, size_t width) {
#define EDGE_SET (hash_new(128, hash_int, hash_cmp_int))
#define ON_EDGE(row, col) (!hash_hask(plot, POS(row, col)))
#define HAS(row, col) (hash_hask(plot, POS(row, col)))
#define MARK_LINE(ix, erow, ecol, drow, dcol) \
  if (ON_EDGE(row + erow, col + ecol) && !hash_hask(edges[ix], POS(row + erow, col + ecol))) { \
      for (int sign = 1; abs(sign) == 1; sign -= 2) \
        for (int d = 0; HAS(row + drow * d, col + dcol * d); d += sign) \
          if (ON_EDGE(row + erow + drow * d, col + ecol + dcol * d)) \
            hash_set(edges[ix], POS(row + erow + drow * d, col + ecol + dcol * d), KV(.int32 = 1)); \
          else break; \
      edge_count++; \
  }
  size_t edge_count = 0;
  hashtable *edges[] = {EDGE_SET, EDGE_SET, EDGE_SET, EDGE_SET};
  FOREACH_KV(plot, {
    UNPOS(key, row, col);
    MARK_LINE(0, 1, 0, 0, 1);
    MARK_LINE(1, -1, 0, 0, 1);
    MARK_LINE(2, 0, 1, 1, 0);
    MARK_LINE(3, 0, -1, 1, 0);
  });
  for (int i = 0; i < SIZEARR(edges); i++) {
    hash_free(edges[i]);
  }
  return edge_count;
}

int main(int argc, char **argv) {
  char *map[256];
  size_t height = read_lines(argv[1], map, SIZEARR(map));
  size_t width = strlen(*map);
  uint64_t part1 = 0, part2 = 0;
  deque *plots = find_plots(map, height, width);
  FOREACH_V(plots, {
    hashtable *plot = val.ptr;
    uint64_t area = hash_len(plot);
    uint64_t perimeter = calc_perimeter(plot, map, height, width);
    uint64_t edges = calc_edges(plot, map, height, width);
    part1 += area * perimeter;
    part2 += area * edges;
  });
  printf("Part 1: [%ld]\n", part1);
  printf("Part 2: [%ld]\n", part2);
  free_lines(map, height);
  FOREACH_V(plots, { hash_free(val.ptr); });
  deq_free(plots);
  return 0;
}