#ifndef XAX_SCAN_H
#define XAX_SCAN_H

#include <stdint.h>
#include <stdlib.h>

#define SCAN(block, results, filter) \
  size_t results##_arr_size = 128;                                                                                    \
  uintptr_t results[results##_arr_size];                                                                               \
  size_t results##_count = 0;                                                                                         \
  do {                                                                                                                \
    mem_block *blk = (block);                                                                                         \
    char *mem = blk->bytes;                                                                                            \
    size_t ml = blk->size;                                                                                            \
    uintptr_t base = blk->base_addr;                                                                                   \
    for (uintptr_t offset = 0; offset < ml - 3 && results##_count < results##_arr_size; offset++) {                       \
      uint word32 = (mem[offset] << 24) | (mem[offset + 1] << 16) | (mem[offset + 2] << 8) | mem[offset + 3];          \
      uint int32 = ile(word32);                                                                                       \
      float float32 = fle(word32);                                                                                    \
      int ret = 0;                                                                                                    \
      filter                                                                                                          \
      if (ret) {                                                                                                      \
        results[results##_count++] = base + offset;                                                                    \
      }                                                                                                               \
    }                                                                                                                 \
  } while (0)                                                                                                         \


int ile(uint word32);

float fle(uint word32);

#endif //XAX_SCAN_H
