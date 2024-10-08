#ifndef XAX_SCAN_H
#define XAX_SCAN_H

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

union word64 {
  char bytes[8];
  int int32;
  float float32;
  long long int64;
  double float64;
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

#define WORD_ADDR (base_addr + offset)

#endif //XAX_SCAN_H
