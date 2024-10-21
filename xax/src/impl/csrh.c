#include <stdio.h>
#include <unistd.h>
#include "args.h"
#include "proc.h"
#include "util.h"
#include "impl.h"
#include "hashtable.h"
#include <pthread.h>

#define BLOCKS 256

static const int OFFSET_X = 66;
static int SAMPLE_PATTERN_X[] =
  {128, 63, 2, -1, 3, 128, -1, -1, -1, -1, 13};

static int OFFSET_Ps = -800;
static int SAMPLE_PATTERN_Ps[] =
  {97, -1, -1, 252, -1, -1, -1, -1, 208, -1, -1, -1, -1, -1, -1, -1, 64, 112, 18};

pthread_mutex_t LOCK;
hashtable *MY_XS;
hashtable *PS_XS;

static int coord_legit(float xy) {
  return IN_RANGE(-5000, xy, 5000) && !FLOAT_EQ(xy, 0) && !is_div_by(xy, 0.000250);
}

static void *run_bg_scans(void *) {
  OPEN_MEM("cs2$");
  READ_DS(BLOCKS);
  mem_block *bs[BLOCKS];
  for (;;) {
    for (int i = 100; i < 180; i++) {
      mem_desc desc = ds[i];
      bs[i] = read_mem_block(fd, desc.start, desc.size);
      char *bytes = bs[i]->bytes;
      for (size_t j = 0; j < bs[i]->size; j++) {
        uintptr_t x_addr = desc.start + j + OFFSET_X;
        if (matches(bytes + j, SAMPLE_PATTERN_X, SIZEARR(SAMPLE_PATTERN_X))
            && !hash_hask(MY_XS, KV(.uint64 = x_addr))) {
          hash_set(MY_XS, KV(.uint64 = x_addr), KV(.uint64 = 0));
        }
        uintptr_t p_addr = desc.start + j + OFFSET_Ps;
        if (matches(bytes + j, SAMPLE_PATTERN_Ps, SIZEARR(SAMPLE_PATTERN_Ps))
            && !hash_hask(PS_XS, KV(.uint64 = p_addr))) {
          hash_set(PS_XS, KV(.uint64 = p_addr), KV(.uint64 = 0));
        }
      }
      free_mem(bs[i]);
    }
    sleep(30);
  }
}

static void print(int fd, hashtable *tbl) {
  int output = 0;
  size_t len = tbl->len;
  kv *x_keys = hash_keys(tbl);
  len = tbl->len;
  printf("LEN: %ld\n", len);
  for (int i = 0; i < len; i++) {
    float prev_x = hash_getv(tbl, x_keys[i]).float32;
    float curr_x = read_mem_word32(fd, x_keys[i].uint64).float32;
    if (coord_legit(curr_x) && FLOAT_DIFF(curr_x, prev_x, 1)) {
      float curr_y = read_mem_word32(fd, x_keys[i].uint64 + 4).float32;
      if (!coord_legit(curr_y)) {
        continue;
      }
      printf("0x%lx  (%f,%f)\n", x_keys[i].uint64, curr_x, curr_y);
      hash_set(tbl, x_keys[i], KV(.float32 = curr_x));
      output = 1;
    }
  }
}

static void *run_bg_output(void *) {
  OPEN_MEM("cs2$");
  for (;;) {
    print(fd, MY_XS);
    print(fd, PS_XS);
    usleep(1000 * 1000);
  }
}

static void run(void) {
  MY_XS = hash_new(128, hash_int, hash_cmp_int);
  PS_XS = hash_new(1024, hash_int, hash_cmp_int);
  pthread_t bg_scanner, bg_output;
  pthread_create(&bg_scanner, NULL, run_bg_scans, NULL);
  pthread_create(&bg_output, NULL, run_bg_output, NULL);
  pthread_join(bg_scanner, NULL);
  pthread_join(bg_output, NULL);
}


__attribute__((constructor))
static void init(void) {
  args_add("csrh", run);
}
