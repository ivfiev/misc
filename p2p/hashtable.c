#include "p2p.h"

size_t hash_func(void *ptr, size_t N) {
  long long ll = (long long)ptr;
  size_t i = ((ll >> 32) ^ ll);
  return i % N;
}

hashtable *hash_new(size_t cap, int (*cmp)(void *k1, void *k2)) {
  struct node **vs = calloc(cap, sizeof(struct node *));
  hashtable *ht = malloc(sizeof(hashtable));
  ht->nodes = vs;
  ht->cap = cap;
  ht->len = 0;
  ht->cmp = cmp;
  return ht;
}

void *hash_set(hashtable *ht, void *k, void *v) {
  size_t h = hash_func(k, ht->cap);
  struct node *node = ht->nodes[h];
  struct node *prev = NULL;
  while (node) {
    if (!ht->cmp(k, node->key)) {
      void *prev_val = node->val;
      node->val = v;
      return prev_val;
    }
    prev = node;
    node = node->next;
  }
  struct node *new = malloc(sizeof(struct node));
  new->next = NULL;
  new->key = k;
  new->val = v;
  if (!prev) {
    ht->nodes[h] = new;
  } else {
    prev->next = new;
  }
  ht->len++;
  return NULL;
}

void *hash_get(hashtable *ht, void *k) {
  size_t h = hash_func(k, ht->cap);
  struct node *node = ht->nodes[h];
  while (node) {
    if (!ht->cmp(k, node->key)) {
      return node->val;
    }
    node = node->next;
  }
  return NULL;
}

struct node *hash_del(hashtable *ht, void *k) {
  size_t h = hash_func(k, ht->cap);
  struct node *node = ht->nodes[h];
  struct node *prev = NULL;
  while (node) {
    if (!ht->cmp(k, node->key)) {
      if (!prev) {
        ht->nodes[h] = node->next;
      } else {
        prev->next = node->next;
      }
      ht->len--;
      return node;
    }
    prev = node;
    node = node->next;
  }
  return NULL;
}

void hash_foreach(hashtable *ht, void (*f)(int i, void *k, void *v)) {
  for (int n = 0, i = 0; n < ht->cap; n++) {
    struct node *node = ht->nodes[n];
    while (node) {
      f(i, node->key, node->val);
      node = node->next;
      i++;
    }
  }
}

void hash_free(hashtable *ht) {
  free(ht->nodes);
  free(ht);
}