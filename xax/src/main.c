#include <string.h>

static char *KEYS[128];

static void (*IMPL[128])(void);

static int IMPL_COUNT;

void add_impl(char *key, void (*impl)(void)) {
  KEYS[IMPL_COUNT] = key;
  IMPL[IMPL_COUNT] = impl;
  IMPL_COUNT++;
}

int main(int argc, char **argv) {
  for (int i = 0; i < IMPL_COUNT; i++) {
    if (strcasestr(KEYS[i], argv[1])) {
      IMPL[i]();
      break;
    }
  }
  return 0;
}
