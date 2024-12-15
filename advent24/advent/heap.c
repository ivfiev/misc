#include "advent.h"

static void sift_up(deque *);
static void sift_down(deque *);

heap *heap_new(void) {
  heap *h = malloc(sizeof(heap));
  h->deq = deq_new();
  return h;
}

size_t heap_len(heap *h) {
  return h->deq->len;
}

void heap_push(heap *h, kv item, long priority) {
  heap_node *node = malloc(sizeof(heap_node));
  node->item = item;
  node->priority = priority;
  deq_push_tail(h->deq, KV(.ptr = node));
  sift_up(h->deq);
}

kv heap_pop(heap *h, long *priority) {
  heap_node *node = deq_pop_head(h->deq).ptr;
  kv item = node->item;
  if (priority != NULL) {
    *priority = node->priority;
  }
  free(node);
  kv last = deq_pop_tail(h->deq);
  deq_push_head(h->deq, last);
  sift_down(h->deq);
  return item;
}

void heap_free(heap *h) {
  FOREACH_V(h->deq, { free(val.ptr); });
  deq_free(h->deq);
  free(h);
}

static void sift_up(deque *deq) {
  size_t curr_ix = deq_len(deq) - 1;
  for (; curr_ix != 0;) {
    size_t parent_ix = (curr_ix - 1) / 2;
    heap_node *curr = deq_at(deq, curr_ix).ptr;
    heap_node *parent = deq_at(deq, parent_ix).ptr;
    if (curr->priority <= parent->priority) {
      break;
    } 
    deq_swap(deq, curr_ix, parent_ix);
    curr_ix = parent_ix;
  }
}

static void sift_down(deque *deq) {
  size_t deqlen = deq_len(deq);
  size_t curr_ix = 0;
  for (;;) {
    size_t left_ix = curr_ix * 2 + 1;
    size_t right_ix = curr_ix * 2 + 2;
    if (left_ix >= deqlen) {
      break;
    }
    heap_node *curr = deq_at(deq, curr_ix).ptr;
    heap_node *left = deq_at(deq, left_ix).ptr;
    heap_node *right = deq_at(deq, right_ix).ptr;
    size_t best_ix = right_ix;
    heap_node *best = right;
    if (left->priority > right->priority) {
      best_ix = left_ix;
      best = left;
    }
    if (curr->priority >= best->priority) {
      break;
    }
    deq_swap(deq, curr_ix, best_ix);
    curr_ix = best_ix;
  }
}