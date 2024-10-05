#ifndef XAX_SCAN_H
#define XAX_SCAN_H

#define SCAN(block, results, results_len, filter)                                                                     \
  size_t results[results_len];                                                                                        \
  size_t results##_count = 0;                                                                                         \
  size_t results##_length = results_len;                                                                              \
  do {                                                                                                                \
    mem_block *blk = (block);                                                                                         \
    char *mem = blk->bytes;                                                                                            \
    size_t ml = blk->size;                                                                                            \
    size_t off = blk->base_addr;                                                                                       \
    for (size_t mp = 0; mp < ml - 3 && results##_count < results##_length; mp++) {                                  \
      uint word32 = (mem[mp] << 24) | (mem[mp + 1] << 16) | (mem[mp + 2] << 8) | mem[mp + 3];                        \
      uint int32 = ile(word32);                                                                                       \
      float float32 = fle(word32);                                                                                    \
      int ret = 0;                                                                                                    \
      filter                                                                                                          \
      if (ret) {                                                                                                      \
        results[results##_count++] = off + mp;                                                                        \
      }                                                                                                               \
    }                                                                                                                 \
  } while (0)                                                                                                         \


int ile(uint word32);

float fle(uint word32);

#endif //XAX_SCAN_H
