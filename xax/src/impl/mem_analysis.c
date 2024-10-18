#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include "args.h"
#include "util.h"
#include "mem.h"
#include "hashtable.h"

char *FILENAMES[MAX_FILES];
hashtable *BYTES[MAX_FILES];

static int filenames(void) {
  char key[8];
  int i;
  for (i = 0; i < SIZEARR(FILENAMES); i++) {
    snprintf(key, 8, "arg%d", i);
    char *filename = args_get(key);
    if (!strcmp(filename, "")) {
      break;
    }
    FILENAMES[i] = filename;
  }
  return i;
}

void readfiles(void) {
  char line_buf[32 * 1024];
  char *byte_toks[SAMPLE_SIZE];
  for (int file = 0; FILENAMES[file]; file++) {
    BYTES[file] = hash_new(MAX_LINES, hash_int, hash_cmp_int);
    FILE *fs = fopen(FILENAMES[file], "r");
    for (int line = 0; fgets(line_buf, SIZEARR(line_buf), fs); line++) {
      size_t size = strsplit(line_buf, " ", byte_toks, SIZEARR(byte_toks));
      uint8_t *bytes = calloc(size, sizeof(uint8_t));
      hash_set(BYTES[file], KV(.int32 = line), KV(.ptr = bytes));
      for (int i = 0; i < size; i++) {
        uint8_t byte = (uint8_t)strtol(byte_toks[i], NULL, 16);
        bytes[i] = byte;
      }
    }
  }
}

static int CURR_LINE_IXS[16];
static int MAX_SCORE;
static int MAX_SCORE_LINE_IXS[16];

int score(int depth) {
  if (!BYTES[depth]) {
    return -1;
  }
  int curr_score = 0, j = 1;
  uint8_t *bytes[16];
  for (int line = 0; line < BYTES[depth]->len; line++) {
    CURR_LINE_IXS[depth] = line;
    if (score(depth + 1) < 0) {
      for (j = 0; j < depth; j++) {
        bytes[j] = hash_getv(BYTES[j], KV(.int32 = CURR_LINE_IXS[j])).ptr;
      }
      for (int i = 0; i < SAMPLE_SIZE; i++) {
        for (j = 1; j < depth; j++) {
          if (bytes[0][i] != bytes[j][i]) {
            break;
          }
        }
        if (j == depth) {
          curr_score++;
        }
      }
      if (curr_score > MAX_SCORE) {
        MAX_SCORE = curr_score;
        for (int i = 0; i <= depth; i++) {
          MAX_SCORE_LINE_IXS[i] = CURR_LINE_IXS[i];
        }
      }
    }
  }
  return curr_score;
}

void printsig(void) {
  for (int i = 0; i < SAMPLE_SIZE; i++) {
    int j;
    uint8_t byte0, byte1;
    for (j = 0; BYTES[j + 1]; j++) {
      byte0 = ((uint8_t *)hash_getv(BYTES[j], KV(.int32 = MAX_SCORE_LINE_IXS[j])).ptr)[i];
      byte1 = ((uint8_t *)hash_getv(BYTES[j + 1], KV(.int32 = MAX_SCORE_LINE_IXS[j + 1])).ptr)[i];
      if (byte0 != byte1) {
        break;
      }
    }
    if (!BYTES[j + 1]) {
      printf("%d ", byte0);
    } else {
      printf("-1 ");
    }
  }
  puts("");
}

static void sigscan(void) {
  filenames();
  readfiles();
  score(0);
  printsig();
}

__attribute__((constructor))
static void init(void) {
  args_add("sigscan", sigscan);
}
