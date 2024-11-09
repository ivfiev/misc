#include <unistd.h>
#include "util.h"
#include "types.h"
#include "scan.h"
#include "proc.h"
#include <string.h>
#include <stdio.h>

#define MIN_PTR 0x500000000000
#define MAX_PTR 0x800000000000

size_t entries_filter(pid_t pid, int fd, struct entry *entries, size_t size) {
  size_t entry_ix = 0;
  FOREACH_BLOCK(1, 2000, {
    SCAN(block, {
      if (entry_ix >= size) {
        perror("need moar MEMORY");
        exit(1);
      }
      if (IN_RANGE(MIN_PTR, word.ptr64, MAX_PTR)) {
        entries[entry_ix].addr = WORD_ADDR;
        entries[entry_ix++].val = word.ptr64;
      }
    });
  });
  return entry_ix;
}

static int cmp_entries(const void *v1, const void *v2) {
  struct entry *e1 = (struct entry *)v1;
  struct entry *e2 = (struct entry *)v2;
  if (e1->val < e2->val) {
    return -1;
  } else if (e1->val > e2->val) {
    return 1;
  } else {
    return 0;
  }
}

void entries_sort(struct entry *entries, size_t size) {
  qsort(entries, size, sizeof(struct entry), cmp_entries);
}

size_t entries_bsearch(uintptr_t addr, struct entry *entries, size_t size) {
  size_t i = 0, j = size;
  while (j - i > 2) {
    int mid = (i + j) / 2;
    if (entries[mid].val < addr) {
      i = mid;
    } else {
      j = mid + 1;
    }
  }
  while (i < size && entries[i].val < addr) {
    i++;
  }
  return i;
}