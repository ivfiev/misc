#ifndef DEQUE_H
#define DEQUE_H

#include "advent.h"

#define FOREACH_V(deq, code) \
  do { \
    size_t length = deq_len(deq); \
    for (size_t deq_ix = 0; deq_ix < length; deq_ix++) { \
      kv val = deq_at(deq, deq_ix); \
      code \
    } \
  } while(0)

deque *deq_new(void);

size_t deq_len(deque *);

deque *deq_copy(deque *);

kv deq_at(deque *, size_t);

kv deq_head(deque *deq);

kv deq_tail(deque *deq);

void deq_push_tail(deque *, kv);

void deq_push_head(deque *, kv);

kv deq_pop_head(deque *);

kv deq_pop_tail(deque *);

void deq_free(deque *);

#endif