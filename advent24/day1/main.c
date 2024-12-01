#include "advent.h"
#include "hashtable.h"

int main(int argc, char **argv) {
  char *lines[1024];
  size_t count = read_lines(argv[1], lines, 1024);
  int *ns = calloc(count, sizeof(int));
  int *ms = calloc(count, sizeof(int));
  uint64_t sum = 0;
  for (int i = 0; i < count; i++) {
    int n, m;
    sscanf(lines[i], "%d %d", &n, &m);
    ns[i] = n;
    ms[i] = m;
  }
  qsort(ns, count, sizeof(int), int_cmp);
  qsort(ms, count, sizeof(int), int_cmp);
  
  for (int i = 0; i < count; i++) {
    sum += abs(ns[i] - ms[i]);
  }
  printf("Part 1: [%ld]\n", sum);

  sum = 0;
  hashtable *tbl = hash_new(100000, hash_int, hash_cmp_int);
  for (int i = 0; i < count; i++) {
    hash_set(tbl, KV(.int32 = ns[i]), KV(.int32 = 0));
  }
  for (int i = 0; i < count; i++) {
    if (hash_hask(tbl, KV(.int32 = ms[i]))) {
      int val = hash_getv(tbl, KV(.int32 = ms[i])).int32;
      hash_set(tbl, KV(.int32 = ms[i]), KV(.int32 = val + 1));
    }
  }
  FOREACH_KV(tbl, {
    sum += key.int32 * val.int32;
  });
  printf("Part 2: [%ld]\n", sum);

  free_lines(lines, count);
  hash_free(tbl);
  free(ns);
  free(ms);
  return 0;
}