#include <unistd.h>
#include <string.h>
#include <stdio.h>
#include "proc.h"
#include "util.h"
#include "scan.h"
#include "args.h"
#include "hashtable.h"

static char *MAPS[] = {"heap", "stack"};

static hashtable *ADDR_INFO;

typedef struct addr_info {
  int i;
  float history[8];
} addr_info;

static void init_state(void) {
  if (ADDR_INFO != NULL) {
    hash_free(ADDR_INFO);
  }
  ADDR_INFO = hash_new(1024 * 32, hash_int, hash_cmp_int);
}

static void filter(mem_block *block) {
  SCAN(block, {
    if (word.float32 != 0 && word.float32 <= 1.1 && word.float32 >= -1.1) {
      addr_info *info = malloc(sizeof(addr_info));
      memset(info, 0, sizeof(addr_info));
      hash_set(ADDR_INFO, KV(.uint64 = WORD_ADDR), KV(.ptr = info));
    }
  });
  printf("filtered %zu addresses\n", ADDR_INFO->len);
}

static void run() {
  init_state();
  mem_desc ds[32];
  pid_t pid = get_pid("sample");
  size_t ds_count = read_mem_desc(pid, ds, SIZEARR(ds));
  int fd = open_mem(pid);

  for (int i = 0; i < SIZEARR(MAPS); i++) {
    int desc_ix = find_mem_desc(MAPS[i], ds, ds_count);
    mem_desc desc = ds[desc_ix];
    mem_block *block = read_mem_block(fd, desc.start, desc.size);
    filter(block);
    free_mem(block);
  }

  kv *keys = hash_keys(ADDR_INFO);

  for (int j = 0; j < 3; j++) {
    for (int i = 0; i < SIZEARR(MAPS); i++) {
      int desc_ix = find_mem_desc(MAPS[i], ds, ds_count);
      mem_desc desc = ds[desc_ix];
      mem_block *block = read_mem_block(fd, desc.start, desc.size);
      SCAN(block, {
        kv k = KV(.uint64 = WORD_ADDR);
        if (hash_hask(ADDR_INFO, k)) {
          addr_info *info = hash_getv(ADDR_INFO, k).ptr;
          info->history[info->i++] = word.float32;
        }
      });
    }
    sleep(1);
  }

  for (int i = 0; i < ADDR_INFO->len; i++) {
    addr_info *info = hash_getv(ADDR_INFO, KV(.uint64=keys[i].uint64)).ptr;
    int j;
    for (j = 0; j < info->i; j++) {
      if (!IN_RANGE(-1.1, info->history[j], 1.1)) {
        break;
      }
    }
    if (j != info->i) {
      continue;
    }
    for (j = 0; j < info->i - 1; j++) {
      if (FLOAT_DIFF(info->history[j], info->history[j + 1], 0.1)) {
        printf("%lx %f %f %f \n", keys[i].uint64, info->history[0], info->history[1], info->history[2]);
        break;
      }
    }
  }

  close_mem(fd);
}

__attribute__((constructor))
static void init(void) {
  args_add("sample", run);
}