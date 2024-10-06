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

  SCAN(block, offsets, {
    ret = int32 == 0x7921 || 123 < float32 && float32 < 124;
  });

  for (int i = 0; i < offsets_count; i++) {
    printf("%zx\n", offsets[i]);
  }

  free_mem(block);
  close_mem(fd);
}

__attribute__((constructor))
void init(void) {
  add_impl("xax_train", run);
}