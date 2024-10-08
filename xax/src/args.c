#include <string.h>
#include <stdio.h>
#include "hashtable.h"

static hashtable *ARGS;

static void assert_init(void) {
  if (ARGS == NULL) {
    ARGS = hash_new(32, hash_str, hash_cmp_str);
  }
}

void args_add(char *key, void *val) {
  assert_init();
  hash_set(ARGS, KV(.str=key), KV(.ptr=val));
}

char *args_get(char *key) {
  return hash_getv(ARGS, KV(.str = key)).str;
}

void args_exec(char *key) {
  kv v = hash_getv(ARGS, KV(.str=key));
  ((void (*)(void))v.ptr)();
}

void args_parse(int argc, char **args) {
  char buf[16];
  for (int i = 0; i < argc; i++) {
    snprintf(buf, sizeof(buf), "arg%d", i);
    char *ix = strdup(buf);
    hash_set(ARGS, KV(.str = ix), KV(.str = args[i]));
  }
}