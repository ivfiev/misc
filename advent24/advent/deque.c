#include "advent.h"

#define DEFAULT_SIZE 8
#define INDEX(i) ((i) < 0  \
  ? (deq->head >= (-i) ? deq->head + (i) : deq->cap + deq->head + (i)) \
  : (deq->head + (i) < deq->cap ? deq->head + (i) : (i) - deq->cap + deq->head))

size_t deq_len(deque *);

static deque *resize(deque *deq) {
  puts("resize");
  size_t len = deq_len(deq);
  size_t cap = deq->cap;
  if (len < cap / 4) {
    cap /= 2;
  } else if (len > cap / 2) {
    cap *= 2;
  }
  if (cap != deq->cap) {
    kv *new_items = calloc(cap, sizeof(kv));
    for (size_t i = 0; i < deq_len(deq); i++) {
      new_items[i] = deq_at(deq, i);
    }
    free(deq->items);
    deq->items = new_items;
    deq->head = 0;
    deq->cap = cap;
  }
  return deq;
}

deque *deq_new(void) {
  deque *deq = malloc(sizeof(deque));
  *deq = (deque){
    .items = calloc(DEFAULT_SIZE, sizeof(kv)), 
    .cap = DEFAULT_SIZE, 
    .head = 0,
    .len = 0};
  return deq;
}

size_t deq_len(deque *deq) {
  return deq->len;
}

kv deq_at(deque *deq, size_t ix) {
  return deq->items[INDEX(ix)];
}

kv deq_head(deque *deq) {
  return deq->items[deq->head];
}

kv deq_tail(deque *deq) {
  return deq->items[deq->head + deq->len - 1];
}

void deq_push_head(deque *deq, kv val) {
  if (deq->len == deq->cap) {
    resize(deq);
  }
  deq->head = INDEX(-1);
  deq->items[deq->head] = val;
  deq->len++;
}

void deq_push_tail(deque *deq, kv val) {
  if (deq->len == deq->cap) {
    resize(deq);
  } 
  deq->items[INDEX(deq->len)] = val;
  deq->len++;
}

kv deq_pop_head(deque *deq) {
  if (deq->len < deq->cap / 4) {
    resize(deq);
  }
  kv val = deq->items[INDEX(0)];
  deq->head = INDEX(1);
  deq->len--;
  return val;
}

kv deq_pop_tail(deque *deq) {
  if (deq->len < deq->cap / 4) {
    resize(deq);
  }
  kv val = deq->items[INDEX(deq->len - 1)];
  deq->len--;
  return val;
}

void deq_free(deque *deq) {
  free(deq->items);
  free(deq);
} 
