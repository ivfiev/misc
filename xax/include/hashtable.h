#ifndef XAX_HASHTABLE_H
#define XAX_HASHTABLE_H

#include <stddef.h>
#include <stdint.h>

#define KV(def) ((kv) {def})

typedef union keyval_t {
  void *ptr;
  char *str;
  uint64_t uint64;
  int int32;
  float float32;
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

hashtable *hash_new(size_t cap, uint64_t (*hash)(kv k, size_t N), int (*cmp)(kv k1, kv k2));

void hash_set(hashtable *ht, kv k, kv v);

int hash_hask(hashtable *ht, kv k);

kv hash_getv(hashtable *ht, kv k);

void hash_del(hashtable *ht, kv k);

kv *hash_keys(hashtable *ht);

void hash_free(hashtable *ht);

uint64_t hash_int(kv k, size_t N);

uint64_t hash_str(kv k, size_t N);

int hash_cmp_str(kv k1, kv k2);

int hash_cmp_int(kv k1, kv k2);

#endif