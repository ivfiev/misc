#pragma clang diagnostic push
#pragma ide diagnostic ignored "UnusedValue"

#include <stdio.h>
#include <unistd.h>
#include "args.h"
#include "proc.h"
#include "util.h"
#include "scan.h"
#include "hashtable.h"

static void run(void) {
  mem_desc ds[4096];
  pid_t pid = get_pid("h3sod");
  size_t ds_count = read_mem_desc(pid, ds, SIZEARR(ds)) - 1000;
  int fd = open_mem(pid);
//  mem_desc *desc = find_mem_desc("/home/fi/Downloads/Heroes", ds, ds_count);
//  uint64_t offset = 0x20ba4;
//  uintptr_t addr = 0x5500ba4;
//  write_mem(fd, addr, (union word64){.int32=9999999}.bytes, 4);
  for (int i = 0; i < ds_count - 1; i++) {
    mem_desc *desc = ds + i;
    mem_block *block = read_mem(fd, desc->start, desc->size);
    SCAN(block, {
      if (word.int32 == 14675) {
        printf("%s -> %lx, %lx\n", desc->name, WORD_ADDR, offset);
      }
    });
    free_mem(block);
  }
  printf("sleeping... go sell some wood!\n");
  sleep(10);
  for (int i = 0; i < ds_count - 1; i++) {
    mem_desc *desc = ds + i;
    mem_block *block = read_mem(fd, desc->start, desc->size);
    SCAN(block, {
      if (word.int32 == 14700) {
        printf("%s -> %lx, %lx\n", desc->name, WORD_ADDR, offset);
      }
    });
    free_mem(block);
  }
}

__attribute__((constructor))
static void init(void) {
  args_add("h3sod", run);
}


#pragma clang diagnostic pop