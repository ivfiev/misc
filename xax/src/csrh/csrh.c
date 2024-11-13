#include <stdio.h>
#include <unistd.h>
#include "args.h"
#include "proc.h"
#include "util.h"
#include "impl.h"
#include "scan.h"
#include <string.h>

uintptr_t hop(int mem_fd, uintptr_t base, uintptr_t offsets[], size_t size) {
  for (int i = 0; i < size - 1; i++) {
    base = read_mem_word64(mem_fd, base + offsets[i]).ptr64;
  }
  return base + offsets[size - 1];
}

size_t get_ctls(int mem_fd, uintptr_t base, uintptr_t ctls[]) {
  int i = 0;
  uintptr_t global_state = read_mem_word64(mem_fd, base + 0x3a2f5b0 - 0x100000).ptr64;
  uintptr_t player_list = read_mem_word64(mem_fd, global_state + 0x10).ptr64;
  for (int player = 1; player <= 64; player++) {
    uintptr_t ctl_ptr_ptr = player_list + player * 0x78;
    uintptr_t ctl_ptr = read_mem_word64(mem_fd, ctl_ptr_ptr).ptr64;
    if (ctl_ptr != 0) {
      ctls[i++] = ctl_ptr;
    }
  }
  return i;
}

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

        uintptr_t offsets[] = {0x618, 0xfd0, 0x2b0, 0xe8, 0xd58, 0x740, 0x9e0, 0xf40, 0x148, 0x48, 0x0, 0xa38, 0xa38, 0x30,};
        uintptr_t ptr_x = hop(fd, deref, offsets, SIZEARR(offsets));
        float x = read_mem_word32(fd, ptr_x).float32;
        float y = read_mem_word32(fd, ptr_x + 4).float32;
        printf("%f %f         0x%lx\n", x, y, ptr_x);
      }
    }
    msleep(425);
  }
}

__attribute__((constructor))
static void init(void) {
  args_add("csrh", run);
}
// use gtk/imgui or make tk bearable