#include "p2p.h"

void err(const char *msg) {
  printf("%s\n", msg);
  printf("%s\n", strerror(errno));
  exit(1);
}