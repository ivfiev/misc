#include <stdio.h>
#include <unistd.h>
#include "args.h"
#include "proc.h"
#include "util.h"
#include "scan.h"
#include "impl.h"

#define BLOCKS 4096

int PATTERN[] = {176, 68, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 128, 63, -1,
                 -1, 128, 63};

static void run(void) {
  OPEN_MEM("cs2$");
//  char buf[4000];
//  read_mem_bytes(fd, 0x788cca148338 - 822, buf, 4000);
//  for (int i = 0; i < 200; i++) {
//    printf("%d ", (uint8_t)buf[i]);
//  }
//  puts("");
//  for (int j = 0; j < 20; j++) {
//    int k = 0;
//    while (k < SIZEARR(PATTERN) && (PATTERN[k] == -1 || PATTERN[k] == (uint8_t)buf[j + k])) {
//      k++;
//    }
//    printf("K: %d\n", k);
//    if (k == SIZEARR(PATTERN)) {
//      int first_byte = j + 819;
//      union word32 x_pos;
//      union word32 y_pos;
//      for (int n = 0; n < 4; n++) {
//        x_pos.bytes[n] = buf[first_byte + n];
//        y_pos.bytes[n] = buf[first_byte + n + 4];
//      }
//      printf("%f %f\n", x_pos.float32, y_pos.float32);
//      return;
//    }
//  }
//
//  return;
  READ_DS(BLOCKS);
  size_t I = 0;
  mem_block *blocks[BLOCKS];
  for (int i = 1; i < BLOCKS; i++) {
    mem_desc *desc = ds + i;
//    if (!(desc->start <= 0x788cca148338 && 0x788cca148338 <= desc->start + desc->size)) {
//      continue;
//    }
    //puts("that's the one!");
    blocks[i] = read_mem_block(fd, desc->start, desc->size);
    char *bytes = blocks[i]->bytes;
    for (long j = 0; j < blocks[i]->size; j++) {
      int k = 0;
//      if (j + desc->start == 0x788cca148338) {
//        printf("here\n");
//      }
      while (k < SIZEARR(PATTERN) && (PATTERN[k] == -1 || PATTERN[k] == (uint8_t)bytes[j + k])) {
        k++;
      }
      if (k > 100) {
        printf("%d\n", k);
      }
      if (k == SIZEARR(PATTERN)) {
        int first_byte = j + 819;
        union word32 x_pos;
        union word32 y_pos;
        union word32 angle;
        for (int n = 0; n < 4; n++) {
          x_pos.bytes[n] = bytes[first_byte + n];
          y_pos.bytes[n] = bytes[first_byte + n + 4];
//          angle.bytes[n] = bytes[first_byte + n + 12];
        }
        printf("%f %f\n", x_pos.float32, y_pos.float32);
        return;
      }
    }
    free_mem(blocks[i]);
  }
}

__attribute__((constructor))
static void init(void) {
  args_add("csrh", run);
}