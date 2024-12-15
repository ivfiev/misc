#include "advent.h"
#include <pthread.h>

static void trim_end(char *str) {
  int i = strlen(str) - 1;
  while (i > 0 && str[i] == '\n') {
    str[i--] = 0;
  }
}

size_t read_lines(const char *file, char **lines, size_t size) { 
  size_t count = 0;
  char buf[1024 * 32];
  FILE *fp = fopen(file, "r");
  if (fp == NULL) {
    puts(strerror(errno));
    exit(1);
  }
  while (count < size && fgets(buf, sizeof(buf), fp)) {
    trim_end(buf);
    lines[count++] = strdup(buf);
  }
  fclose(fp);
  return count;
}

void free_lines(char **lines, size_t size) {
  for (int i = 0; i < size; i++) {
    free(lines[i]);
  }
}

int int_cmp(const void *ptr_i, const void *ptr_j) {
  return *(int *)ptr_i - *(int *)ptr_j;
}

size_t strsplit(char *str, const char *sep, char **toks, size_t size) {
  size_t i = 0;
  char *save_ptr;
  char *tmp = strtok_r(str, sep, &save_ptr);
  for (toks[i++] = tmp; i < size && tmp != NULL;) {
    tmp = strtok_r(NULL, sep, &save_ptr);
    if (tmp != NULL) {
      toks[i++] = tmp;
    }
  }
  return i;
}

void parallel(void *args, size_t arg_size, size_t args_count, pfunc f) {
  pthread_t ts[args_count];
  int err;
  for (int i = 0; i < args_count; i++) {
    if ((err = pthread_create(ts + i, NULL, (void *(*)(void *))f, args + arg_size * i)) != 0) {
      fprintf(stderr, "pthread_create [%s]\n", strerror(err));
    }
  }
  for (int i = 0; i < args_count; i++) {
    if ((err = pthread_join(ts[i], NULL)) != 0) {
      fprintf(stderr, "pthread_join [%s]\n", strerror(err));
    }
  }
}