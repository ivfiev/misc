#include "advent.h"

uint64_t possible(char *target, deque *patterns, hashtable *cache) {
  if (*target == 0) {
    return 1;
  }
  if (hash_hask(cache, KV(.ptr = target))) {
    return hash_getv(cache, KV(.ptr = target)).uint64;
  }
  size_t sum = 0;
  FOREACH_V(patterns, {
    char *pattern = val.ptr;
    size_t len = strlen(pattern);
    if (strncmp(target, pattern, len) == 0) {
      sum += possible(target + len, patterns, cache);
    }
  });
  hash_set(cache, KV(.ptr = target), KV(.uint64 = sum));
  return sum;
}

int main(int argc, char **argv) {
  char *input[1024];
  size_t lines = read_lines(argv[1], input, SIZEARR(input));
  deque *patterns = deq_new();
  hashtable *cache = hash_new(256, hash_str, hash_cmp_str);
  uint64_t part1 = 0, part2 = 0;
  PARSE_LINE(input[0], " ", 1024, {
    size_t len = strlen(token);
    if (token[len - 1] == ',') token[len - 1] = 0;
    deq_push_tail(patterns, KV(.ptr = token));
  });
  for (int i = 2; i < lines; i++) {
    uint64_t p = possible(input[i], patterns, cache);
    part1 += p > 0;
    part2 += p;
  }
  printf("Part 1: [%ld]\n", part1);
  printf("Part 2: [%ld]\n", part2);
  deq_free(patterns);
  free_lines(input, lines);
  return 0;
}