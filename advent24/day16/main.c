#include "advent.h"

#define MAX_SCORE 0x7FFFFFFFFFFFFFFF
#define NODE(name, row, col, dir) \
  kv name; \
  name.vec16[0] = row; \
  name.vec16[1] = col; \
  name.vec16[2] = dir;
#define UNODE(node, row, col, dir) \
  short row = node.vec16[0]; \
  short col = node.vec16[1]; \
  short dir = node.vec16[2];
#define POP_MIN(tbl, node, score) \
  kv node; \
  uint64_t score = MAX_SCORE; \
  FOREACH_KV(tbl, { \
    if (val.uint64 < score) { \
      score = val.uint64; \
      node = key; \
    } \
  }); \
  hash_del(tbl, node)

int DIRECTIONS[4][2] = {{1, 0}, {0, 1}, {-1, 0}, {0, -1}};

int find(char **maze, size_t height, size_t width, char target, int *row, int *col) {
  for (int i = 0; i < height; i++) {
    for (int j = 0; j < width; j++) {
      if (maze[i][j] == target) {
        *row = i;
        *col = j;
        return 1;
      }
    }
  }
  return 0;
}

hashtable *dijkstra(char **maze, size_t height, size_t width, int start_row, int start_col, int start_dir) {
  hashtable *gray = hash_new(256, hash_int, hash_cmp_int);
  hashtable *black = hash_new(256, hash_int, hash_cmp_int);
  NODE(start, start_row, start_col, start_dir);
  hash_set(gray, start, KV(.uint64 = 0)); 
  while (hash_len(gray) > 0) {
    POP_MIN(gray, node, score);
    hash_set(black, node, KV(.uint64 = score));
    UNODE(node, row, col, dir);
    for (int i = 0; i < SIZEARR(DIRECTIONS); i++) {
      int drow = DIRECTIONS[i][0];
      int dcol = DIRECTIONS[i][1];
      NODE(adj, row + drow, col + dcol, i);
      if (maze[row + drow][col + dcol] != '#' && !hash_hask(black, adj)) {
        uint64_t prev_score = hash_getv(gray, adj).uint64;
        if (prev_score == 0) prev_score = MAX_SCORE;
        hash_set(gray, adj, KV(.uint64 = MIN(prev_score, score + 1 + (i != dir) * 1000)));
      }
    }
  }
  hash_free(gray);
  return black;
}

void paths_to(hashtable *scores, deque *all_paths, deque *curr_path, kv node) {
  if (!hash_hask(scores, node)) {
    return;
  }
  uint64_t score = hash_getv(scores, node).uint64;
  if (score == 0) {
    deque *path = deq_copy(curr_path);
    deq_push_tail(all_paths, KV(.ptr = path));
  } else {
    UNODE(node, row, col, dir);
    for (int i = 0; i < SIZEARR(DIRECTIONS); i++) {
      int drow = DIRECTIONS[i][0];
      int dcol = DIRECTIONS[i][1];
      for (int j = 0; j < SIZEARR(DIRECTIONS); j++) {
        NODE(adj, row - drow, col - dcol, j); 
        uint64_t prev_score = score - 1 - 1000 * (j != dir);
        if (hash_getv(scores, adj).uint64 == prev_score) {
          deq_push_head(curr_path, adj);
          paths_to(scores, all_paths, curr_path, adj);
          deq_pop_head(curr_path);
        }
      }
    }
  }
}

int main(int argc, char **argv) {
  char *maze[512];
  size_t height = read_lines(argv[1], maze, SIZEARR(maze));
  size_t width = strlen(*maze);
  
  // silver
  int start_row, start_col, end_row, end_col;
  find(maze, height, width, 'S', &start_row, &start_col);
  find(maze, height, width, 'E', &end_row, &end_col);
  hashtable *scores = dijkstra(maze, height, width, start_row, start_col, 1);
  uint64_t min_score = MAX_SCORE;
  kv min_node;
  FOREACH_KV(scores, {
    if (key.vec16[0] == end_row && key.vec16[1] == end_col) {
      min_score = MIN(min_score, val.uint64);
      min_node = key;
    }
  });
  printf("Part 1: [%ld]\n", min_score);

  // gold
  deque *all_paths = deq_new();
  deque *path = deq_new();
  deq_push_head(path, min_node);
  hashtable *seats = hash_new(128, hash_int, hash_cmp_int);
  paths_to(scores, all_paths, path, min_node);
  FOREACH_V(all_paths, {
    deque *path = val.ptr;
    FOREACH_V(path, {
      NODE(tile, val.vec16[0], val.vec16[1], 0);
      hash_set(seats, tile, KV(.int32 = 1));
    });
  });
  printf("Part 2: [%ld]\n", hash_len(seats));

  // platinum
  free_lines(maze, height);
  hash_free(scores);
  hash_free(seats);
  FOREACH_V(all_paths, { deq_free(val.ptr); });
  deq_free(all_paths);
  deq_free(path); 
  return 0;
}