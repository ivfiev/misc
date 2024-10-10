#ifndef XAX_SCAN_H
#define XAX_SCAN_H

#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include "proc.h"

union word64 {
  char bytes[8];
  int int32;
  float float32;
  long long int64;
  double float64;
  uint32_t ptr32;
  uintptr_t ptr64;
};

#define SCAN(block, filter) \
  do {                                                                                                                \
    mem_block *blk = (block);                                                                                         \
    char *memory = blk->bytes;                                                                                            \
    size_t memory_size = blk->size;                                                                                            \
    uintptr_t base_addr = blk->base_addr;                                                                                   \
    for (uintptr_t offset = 0; offset < memory_size - 7; offset += 4) {                                                           \
      union word64 word;    \
      memcpy(word.bytes, memory + offset, 8); \
      filter                                                                                                          \
    }                                                                                                                 \
  } while (0)                                                                                                         \

#define FOREACH_BLOCK(code) \
  do {                          \
  mem_desc ds[1536]; \
  size_t ds_size = read_mem_desc(pid, ds, SIZEARR(ds));  \
  for (int i = 0; i < ds_size - 3; i++) { \
    mem_desc desc = ds[i]; \
    mem_block *block = read_mem_block(fd, desc.start, desc.size); \
    code \
    free_mem(block); \
  }                         \
  } while (0)               \

#define WORD_ADDR (base_addr + offset)

#define INFER_TYPE 0
#define FLOAT32_TYPE 1

union word64 parse_value(char *val, int type);

uintptr_t parse_addr(char *val);

int is_int32(union word64 word);

int is_float32(union word64 word);

int is_ptr32(union word64 ptr, mem_desc ds[], size_t ds_size);

#endif
