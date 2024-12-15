#ifndef TYPES_H
#define TYPES_H

#include <stdint.h>
#include <stddef.h>

typedef union keyval_t {
  void *ptr;
  char *str;
  uint64_t uint64;
  int int32;
  float float32;
  int coords[2];
} kv;

typedef struct list_node {
  kv key;
  kv val;
  struct list_node *next;
} list_node;

typedef struct hashtable {
  list_node **nodes;
  size_t cap;
  size_t len;

  int (*cmp)(kv k1, kv k2);

  uint64_t (*hash)(kv k, uint64_t N);
} hashtable;

typedef struct deque {
  kv *items;
  size_t cap;
  size_t head;
  size_t len;
} deque;

typedef struct heap_node {
  long priority;
  kv item;
} heap_node;

typedef struct heap {
  deque *deq;
} heap;

#endif