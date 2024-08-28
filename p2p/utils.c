#include <sys/time.h>
#include <time.h>
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

double randf() {
  return (double)rand() / RAND_MAX;
}

void **rand_select(void **elems, size_t len, size_t k) {
  void **result = calloc(k, sizeof(void *));
  int selected = 0;
  for (int i = 0; selected < k; i++) {
    double prob = (double)(k - selected) / (len - i);
    if (randf() < prob) {
      result[selected++] = elems[i];
    }
  }
  return result;
}

void timestamp(char *buf) {
  struct timeval tv;
  struct tm *tm_info;
  gettimeofday(&tv, NULL);
  tm_info = localtime(&tv.tv_sec);
  size_t c = strftime(buf, 12, "%H:%M:%S", tm_info);
  snprintf(buf + c, 19, ".%03ld", tv.tv_usec / 1000);
}