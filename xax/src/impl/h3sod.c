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
  // x coord (is & cs)
  // 285 0x11e8aa0 0x308aa0 X
  // [7:0x1d8] [0x1501d8] -> [0x11e8a68+0x38]    [str:NULL]

  // x coord (sby!)
  // 285 0x11e8c8c 0x308c8c 14
  // [7:0x1d8] [0x1501d8] -> [0x11e8c38+0x54]    [str:NULL]

  // x coord (blue)
  // [7:0x1d8] [0x1501d8] -> [0x11e8c38+0x38]    [str:NULL]

  // diff L map
  // [7:0x1d8] [0x1501d8] -> [0x11e8c38+0x40]    [str:NULL]

  OPEN_MEM("h3sod");
  READ_DS(BLOCKS);
  union word64 word;
  uintptr_t addr = ds[7].start + 0x1d8;
  printf("0x%lx\n", addr);
  read_mem_bytes(fd, addr, word.bytes, 4);
  addr = word.ptr32 + 0x38;
  printf("0x%lx\n", addr);
  read_mem_bytes(fd, addr, word.bytes, 4);
  printf("%d\n", word.int32);
}

__attribute__((constructor))
static void init(void) {
  args_add("h3sod", run);
}
