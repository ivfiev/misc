#ifndef HEAP_H
#define HEAP_H

#include "advent.h"

heap *heap_new(void);

size_t heap_len(heap *);

void heap_push(heap *, kv val, long priority);

kv heap_pop(heap *, long *priority);

void heap_free(heap *);

#endif