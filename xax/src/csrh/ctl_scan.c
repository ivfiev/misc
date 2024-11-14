#include <stdio.h>
#include <string.h>
#include "hashtable.h"
#include "scan.h"
#include "args.h"
#include "proc.h"
#include "util.h"
#include "impl.h"
#include "math.h"

#define MAX_DEPTH 8
#define BLOCK_SIZE 1024
#define PRECISION 0.01
#define IS_PTR(x) (0x500000000000 < (x) && (x) < 0x800000000000)

size_t get_ctls(uintptr_t ctls[]);

static int MEM_FD;
static uintptr_t LIBCLIENT_BASE;
static hashtable *VISITED;
static uintptr_t CURRENT[MAX_DEPTH];
static uintptr_t CTLS[64];
static float TARGET;
static uintptr_t START;

static void init_set(void) {
  if (VISITED) {
    free(VISITED);
  }
  VISITED = hash_new(BLOCK_SIZE * BLOCK_SIZE, hash_int, hash_cmp_int);
}

static void print_result(int d) {
  hashtable *uniques = hash_new(100, hash_int, hash_cmp_int);
  for (int i = 0; CTLS[i]; i++) {
    uintptr_t ptr = hop(MEM_FD, CTLS[i], CURRENT, d);
    float x = read_mem_word32(MEM_FD, ptr).float32;
    if (fabs(x) > 1 && IN_RANGE(-5000, x, 5000)) {
      hash_set(uniques, KV(.uint64 = (int)x), KV(.uint64 = 1));
    }
  }
  if (uniques->len >= 1) {
    printf("[%zu] ", uniques->len);
    for (int i = 0; i < d; i++) {
      printf("0x%lx, ", CURRENT[i]);
    }
    puts("");
  }
  hash_free(uniques);
}

static void scan(int d, uintptr_t ptr) {
  if (d >= MAX_DEPTH) {
    return;
  }
  char buf[BLOCK_SIZE];
  union word64 word;
  read_mem_bytes(MEM_FD, ptr, buf, SIZEARR(buf));
  for (int i = 0; i < BLOCK_SIZE; i += 4) {
    CURRENT[d] = i;
    memcpy(word.bytes, buf + i, 8);
    if (IN_RANGE(TARGET - PRECISION, word.float32, TARGET + PRECISION)) {
      print_result(d + 1);
    }
    if (IS_PTR(word.ptr64) && !hash_hask(VISITED, KV(.uint64 = word.ptr64))) {
      hash_set(VISITED, KV(.uint64 = word.ptr64), KV(.uint64 = 1));
      scan(d + 1, word.ptr64);
    }
  }
}

void ctl_scan() {
  disable_stderr();
  OPEN_MEM("cs2$");
  READ_DS(1536);
  LIBCLIENT_BASE = get_base_addr(pid, "libclient");
  get_ctls(CTLS);
  MEM_FD = fd;
  START = parse_addr(args_get("arg0"));
  TARGET = parse_value(args_get("arg1"), FLOAT32_TYPE).float32;
  init_set();
  scan(0, START);
}

__attribute__((constructor))
static void init(void) {
  args_add("ctlscan", ctl_scan);
}