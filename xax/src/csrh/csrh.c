#include <stdio.h>
#include <unistd.h>
#include "args.h"
#include "proc.h"
#include "util.h"
#include "impl.h"
#include "scan.h"
#include <string.h>

#define START_ADDR 0x100000
#define GLOBAL_STATE_PTR_ADDR (LIBCLIENT_BASE + 0x3a43f50 - START_ADDR)
#define LOCAL_PLAYER_CTL_PTR_ADDR (LIBCLIENT_BASE + 0x3a3fab8 - START_ADDR)

static int MEM_FD;
static uintptr_t LIBCLIENT_BASE;

struct player_descriptor {
  int is_local;
  float x, y, z;
  float pitch, yaw;
  int is_alive;
  char team;
};

static union word64 read_word(uintptr_t addr) {
  return read_mem_word64(MEM_FD, addr);
}

size_t get_ctls(uintptr_t ctls[]) {
  int i = 0;
  uintptr_t ctl_list = read_word(read_word(GLOBAL_STATE_PTR_ADDR).ptr64 + 0x10).ptr64;
  for (int player = 1; player <= 64; player++) {
    uintptr_t ctl = read_word(ctl_list + player * 0x78).ptr64;
    if (ctl != 0) {
      ctls[i++] = ctl;
    }
  }
  return i;
}

static void read_player(uintptr_t ctl, struct player_descriptor *player) {
  uintptr_t local_ctl = read_word(LOCAL_PLAYER_CTL_PTR_ADDR).ptr64;
  player->is_local = ctl == local_ctl;
  player->is_alive = read_word(ctl + 0x99c).int32;
  player->team = read_word(ctl + 0x55b).int32 == 2 ? 'T' : 'C';
}

static void print_player(struct player_descriptor *player) {
  printf("(%d, %c, %d)\n", player->is_local, player->team, player->is_alive);
}

static void main_loop(void) {
  uintptr_t ctls[64];
  struct player_descriptor player;
  for (;;) {
    size_t ctl_count = get_ctls(ctls);
    for (int i = 0; i < ctl_count; i++) {
      read_player(ctls[i], &player);
      print_player(&player);
    }
    msleep(1000);
  }
}

static void run(void) {
  OPEN_MEM("cs2$");
  READ_DS(1536);
  MEM_FD = fd;
  LIBCLIENT_BASE = get_base_addr(pid, "libclient");
  disable_stderr();
  main_loop();
}

__attribute__((constructor))
static void init(void) {
  args_add("csrh", run);
}
// use gtk/imgui or make tk bearable