#include <stdio.h>
#include <unistd.h>
#include "args.h"
#include "proc.h"
#include "util.h"
#include "impl.h"
#include <string.h>

static void run(void) {
  OPEN_MEM("cs2$");
  READ_DS(1536);
  disable_stderr();
  uintptr_t base = ds[find_mem_desc("libclient", ds, ds_size)].start;
  printf("0x%lx\n", base);
  for (;;) {
    uintptr_t ENTITY_LIST = read_mem_word64(fd, base + 0x3a2f5b0 - 0x100000).ptr64; // param_1 table ptr, param_2 [1..64]
    puts("-----------");
    printf("ENTITY LIST: 0x%lx\n", ENTITY_LIST);
    for (int player = 1; player <= 64; player++) {
      uintptr_t PLAYER_LIST = read_mem_word64(fd, ENTITY_LIST + 0x10).ptr64; // + (long)((int)player >> 9) * 8
      uintptr_t puVar2 = PLAYER_LIST + player * 0x78;
      uintptr_t deref = read_mem_word64(fd, puVar2).ptr64;
      if (deref != 0) {
        printf("PLAYER (game) ix & ptr %d: 0x%lx\n", player - 1, deref);
        printf("PLAYER team: %d\n", read_mem_word32(fd, deref + 0x55b).int32);
        printf("local player: 0x%lx\n", read_mem_word64(fd, base + 0x3a2b118 - 0x100000).ptr64);
        printf("is alive?: %d\n", read_mem_word32(fd, deref + 0x99c).int32);
      }
    }

    msleep(525);
  }
}

__attribute__((constructor))
static void init(void) {
  args_add("csrh", run);
}
// use gtk/imgui or make tk bearable