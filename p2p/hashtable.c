#include "p2p.h"

hashtable *hash_new(size_t cap, size_t (*hash)(void *ptr, size_t N), int (*cmp)(void *k1, void *k2)) {
  struct node **vs = calloc(cap, sizeof(struct node *));
  hashtable *ht = malloc(sizeof(hashtable));
  ht->nodes = vs;
  ht->cap = cap;
  ht->hash = hash;
  ht->len = 0;
  ht->cmp = cmp;
  return ht;
}

void *hash_set(hashtable *ht, void *k, void *v) {
  size_t h = ht->hash(k, ht->cap);
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
  size_t h = ht->hash(k, ht->cap);
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
  size_t h = ht->hash(k, ht->cap);
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

void **hash_keys(hashtable *ht) {
  void **keys = calloc(sizeof(void *), ht->len);
  for (int i = 0, k = 0; i < ht->cap; i++) {
    struct node *node = ht->nodes[i];
    while (node) {
      keys[k++] = node->key;
      node = node->next;
    }
  }
  return keys;
}

void hash_free(hashtable *ht) {
  free(ht->nodes);
  free(ht);
}