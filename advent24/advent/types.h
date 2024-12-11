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

struct node {
  kv key;
  kv val;
  struct node *next;
};

typedef struct hashtable {
  struct node **nodes;
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

#endif