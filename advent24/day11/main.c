#include "advent.h"

#define LEN(x) ((int)log10(x) + 1)
#define SPLIT(x, k, a, b) \
  uint64_t a = x / (uint64_t)pow(10, k); \
  uint64_t b = x % (uint64_t)pow(10, k) \

void hash_add(hashtable *ht, uint64_t key, uint64_t n) {
  hash_set(ht, KV(.uint64 = key), KV(.uint64 = hash_getv(ht, KV(.uint64 = key)).uint64 + n));
}

hashtable *parse_input(char *input, size_t size) {
  char *toks[size];
  size_t count = strsplit(input, " ", toks, size);
  hashtable *stones = hash_new(4096, hash_int, hash_cmp_int);
  for (int i = 0; i < count; i++) {
    uint64_t val = atoll(toks[i]);
    hash_add(stones, val, 1);
  }
  return stones;
}

deque *apply_rule(uint64_t n) {
  deque *ns = deq_new();
  if (n == 0) {
    deq_push_tail(ns, KV(.uint64 = 1));
  } else if (LEN(n) % 2 == 0) {
    SPLIT(n, LEN(n) / 2, left, right);
    deq_push_tail(ns, KV(.uint64 = left));
    deq_push_tail(ns, KV(.uint64 = right));
  } else {
    deq_push_tail(ns, KV(.uint64 = n * 2024));
  }
  return ns;
}

void blink(hashtable **stones) {
  hashtable *new_stones = hash_new(4096, hash_int, hash_cmp_int);
  FOREACH_KV(*stones, {
    uint64_t stone = key.uint64;
    uint64_t count = val.uint64;
    deque *results = apply_rule(stone);
    FOREACH_V(results, {
      uint64_t result = val.uint64;
      hash_add(new_stones, result, count);
    });
    deq_free(results);
  });
  hash_free(*stones);
  *stones = new_stones;
}

uint64_t count(hashtable *stones) {
  uint64_t sum = 0;
  FOREACH_KV(stones, { sum += val.uint64; });
  return sum;
}

int main(int argc, char **argv) {
  char *input[1];
  read_lines(argv[1], input, 1);
  hashtable *stones = parse_input(*input, strlen(*input));
  for (int i = 0; i < 75; i++) {
    blink(&stones);
    if (i == 24) {
      printf("Part 1: [%ld]\n", count(stones));
    }
    if (i == 74) {
      printf("Part 2: [%ld]\n", count(stones));
    }
  }
  free_lines(input, 1);
  hash_free(stones);
  return 0;
}