#include <stdio.h>
#include "args.h"
#include "proc.h"
#include "util.h"
#include "scan.h"
#include "impl.h"

static void run(void) {
  OPEN_MEM("h3sod");
  uintptr_t static_ptr = 0x699538;
  uintptr_t global_offset = 0x20b6c;
  uintptr_t player_offset = 0x168;
  uintptr_t status_offset = 0x1f636;
  union word32 state_pointer;
  read_mem_bytes(fd, static_ptr, state_pointer.bytes, 4);
  uintptr_t resources_ptr = state_pointer.ptr + global_offset;
  union word32 resources[7];
  char status[8];
  read_mem_bytes(fd, state_pointer.ptr + status_offset, status, 8);
  for (int i = 0; i < 8; i++) {
    if (status[i] != 0) {
      continue;
    }
    read_mem_bytes(fd, resources_ptr + i * player_offset, (char *)resources, sizeof(resources));
    printf("Player: %d, Wood: %d, Mercury: %d, Ore: %d, Sulphur: %d, Crystals: %d, Gems: %d, Gold: %d\n", i,
      resources[0].int32, resources[1].int32, resources[2].int32, resources[3].int32, resources[4].int32,
      resources[5].int32, resources[6].int32);
  }
}

__attribute__((constructor))
static void init(void) {
  args_add("h3sod", run);
}
