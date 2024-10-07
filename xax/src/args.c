#include "hashtable.h"

hashtable *ARGS;

static void assert_init(void) {
  if (ARGS == NULL) {
    ARGS = hash_new(32, hash_str, hash_cmp_str);
  }
}

void args_add(char *key, void *val) {
  assert_init();
  hash_set(ARGS, KV(.str=key), KV(.ptr=val));
}

void args_exec(char *key) {
  kv v = hash_getv(ARGS, KV(.str=key));
  ((void (*)(void))v.ptr)();
}