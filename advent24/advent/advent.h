#ifndef ADVENT_H
#define ADVENT_H

#include <stdio.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include "hashtable.h"
#include <errno.h>

size_t read_lines(const char *file, char **lines, size_t size);

void free_lines(char **lines, size_t size);

int int_cmp(const void *ptr_i, const void *ptr_j);

#endif