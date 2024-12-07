#ifndef ADVENT_H
#define ADVENT_H

#include <stdio.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include "hashtable.h"
#include <errno.h>
#include <math.h>

#define SIGN(x) ((x) < 0 ? -1 : (x) > 0 ? 1 : 0)
#define MIN(x, y) ((x) < (y) ? (x) : (y))
#define MAX(x, y) ((x) > (y) ? (x) : (y))
#define SIZEARR(arr) (sizeof(arr) / sizeof(arr[0]))
#define IN_RANGE(a, x, b) ((a) <= (x) && (x) <= (b))

#define PARSE_LINE(line, sep, size, parse) \
  size_t count; \
  do { \
    char *tokens[size]; \
    size_t token_count = strsplit(line, sep, tokens, size); \
    count = token_count; \
    for (int token_ix = 0; token_ix < token_count; token_ix++) { \
      char *token = tokens[token_ix]; \
      parse \
    } \
  } while (0) \

size_t read_lines(const char *file, char **lines, size_t size);

void free_lines(char **lines, size_t size);

int int_cmp(const void *ptr_i, const void *ptr_j);

size_t strsplit(char *str, const char *sep, char **toks, size_t size);

#endif