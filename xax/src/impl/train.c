#include <stdio.h>
#include "proc.h"
#include "util.h"
#include "scan.h"

extern void add_impl(char *, void (*impl)());

static void run() {
  mem_desc ds[32];
  pid_t pid = get_pid("xax_train");
  size_t ds_count = read_mem_desc(pid, ds, SIZEARR(ds));
  mem_desc *desc = find_mem_desc("stack", ds, SIZEARR(ds));
  int fd = open_mem(pid);
  mem_block *block = read_mem(fd, desc->start, desc->size);

  SCAN(block, {
    if (word.int32 == 0x7921) {
      printf("%lx\n", WORD_ADDR);
      union word64 new = {.int32 = 0x7922};
      write_mem(fd, WORD_ADDR, new.bytes, 4);
    }
    if (word.int32 == 0x7922) {
      printf("%lx\n", WORD_ADDR);
      union word64 new = {.int32 = 0x7921};
      write_mem(fd, WORD_ADDR, new.bytes, 4);
    }
    if (123 < word.float32 && word.float32 < 124) {
      printf("%lx\n", WORD_ADDR);
    }
  });

  free_mem(block);
  close_mem(fd);
}

__attribute__((constructor))
void init(void) {
  add_impl("xax_train", run);
}