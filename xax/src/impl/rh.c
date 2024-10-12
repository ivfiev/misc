#include <stdio.h>
#include <unistd.h>
#include "args.h"
#include "proc.h"
#include "util.h"
#include "scan.h"
#include "hashtable.h"
#include "impl.h"

#define BLOCKS 1536

static void run(void) {
  OPEN_MEM("cs2$");
  READ_DS(BLOCKS);
  size_t I = 0;
  mem_block *blocks[BLOCKS];
  for (int i = 1; i < BLOCKS; i++) {
    mem_desc *desc = ds + i;
    blocks[i] = read_mem_block(fd, desc->start, desc->size);
  }
  sleep(1);
  for (;;) {
    uintptr_t from = 0x79260b2bc5a0 - 16;
    uintptr_t to = 0x79260b2bc5a0 + 16;
    char buf[128];
    read_mem_bytes(fd, from, buf, to - from);
    for (uintptr_t i = from; i <= to; i += 4) {
      union word32 x;
      memcpy(x.bytes, buf + (i - from), 4);
      printf("%f\n", x.float32);
    }

//    for (int i = 1; i < BLOCKS; i++) {
//      mem_desc *desc = ds + i;
//      mem_block *block = read_mem_block(fd, desc->start, desc->size);
//      SCAN(block, {
//        union word32 x; // = {.int64 = 0};
//        union word32 y; // = {.int64 = 0};
//        memcpy(x.bytes, memory + offset, 4);
//        memcpy(y.bytes, memory + offset + 4, 4);
//
//        uintptr_t blja = 0x79260b2bc5a0;
//
////        printf("0x%lx [%f]\n", (uintptr_t)memory + offset, word.float32);
//
//        if (ABS((uintptr_t)memory + offset - blja) <= 20) {
//          printf("0x%lx [%f]\n", (uintptr_t)memory, word.float32);
//        }
//
//        if (IN_RANGE(914.02, x.float32, 914.04) || IN_RANGE(2042.02, y.float32, 2042.04)) {
//          union word64 prev_x = {.int64 = 0};;
//          union word64 prev_y = {.int64 = 0};;
//          memcpy(prev_x.bytes, blocks[i]->bytes + offset + 0, 8);
//          memcpy(prev_y.bytes, blocks[i]->bytes + offset + 4, 8);
////          printf("%f %f %f %f\n", x.float32, y.float32, prev_x.float32, prev_y.float32);
////          printf("0x%lx\n", WORD_ADDR);
////          printf("0x%lx 0x%lx\n", (uintptr_t)(memory + offset), (uintptr_t)(memory + offset + 4));
//        }
//
//      });
//      mem_block *tmp = blocks[i];
//      blocks[i] = block;
//      free_mem(tmp);
//    }
    sleep(1);
  }
}

__attribute__((constructor))
static void init(void) {
  args_add("csrh", run);
}