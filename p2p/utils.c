#include "p2p.h"

void trim_end(char *str) {
  while (*str) {
    if (*str == '\n') {
      *str = 0;
    }
    str++;
  }
}

size_t hash_int(void *ptr, size_t N) {
  long long ll = (long long)ptr;
  size_t i = ((ll >> 32) ^ ll);
  return i % N;
}

int intcmp(int i, int j) {
  return i - j;
}

size_t hash_str(void *ptr, size_t N) {
  size_t i = 0;
  char *str = ptr;
  char c;
  while ((c = *str++)) {
    i += i * 89 + (int)c;
  }
  return i % N;
}