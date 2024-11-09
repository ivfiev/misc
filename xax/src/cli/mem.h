#ifndef XAX_MEM_H
#define XAX_MEM_H

#include <stdint.h>
#include "types.h"

#define MAX_FILES 16
#define MAX_LINES 4096
#define SAMPLE_RADIUS 2048
#define SAMPLE_SIZE (2 * SAMPLE_RADIUS + 1)
#define BYTE_STR_LEN (3 * SAMPLE_SIZE + 2)

size_t entries_filter(pid_t pid, int fd, struct entry *entries, size_t size);

void entries_sort(struct entry *entries, size_t size);

size_t entries_bsearch(uintptr_t addr, struct entry *entries, size_t size);

#endif
