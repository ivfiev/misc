#include <unistd.h>
#include <string.h>
#include <stdio.h>
#include "proc.h"
#include "util.h"
#include "scan.h"

extern void add_impl(char *, void (*impl)());

static char *MAPS[] = {"heap", "stack"};

static void run() {
  mem_desc ds[32];
  pid_t pid = get_pid("xax_train");
  size_t ds_count = read_mem_desc(pid, ds, SIZEARR(ds));
  int fd = open_mem(pid);

  for (int i = 0; i < SIZEARR(MAPS); i++) {
    mem_desc *desc = find_mem_desc(MAPS[i], ds, ds_count);
    mem_block *block_old = read_mem(fd, desc->start, desc->size);
    sleep(1);
    mem_block *block = read_mem(fd, desc->start, desc->size);

    // pruning
    SCAN(block, {
      if (word.float32 >= -1 && word.float32 <= 1) {
        union word64 word_old = WORD(block_old->bytes, offset);
        if (word.float32 - word_old.float32 != 0) {
          printf("address [%lx]\n", WORD_ADDR);
          printf("offset [%lx]\n", offset);
        }
      }
    });

    free_mem(block);
  }
  close_mem(fd);
}

__attribute__((constructor))
void init(void) {
  add_impl("xax_train", run);
}