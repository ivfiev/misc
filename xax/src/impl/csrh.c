#include <stdio.h>
#include <unistd.h>
#include "args.h"
#include "proc.h"
#include "util.h"
#include "impl.h"
#include "hashtable.h"
#include <pthread.h>
#include <string.h>

#define BLOCKS 256
#define HISTORY_LEN 8
#define TICKS_PER_SEC 10
#define MAX_SPEED 250.0 + 2.5

struct history {
  float xs[HISTORY_LEN];
  float ys[HISTORY_LEN];
  int i;
  int count;
};

static struct history *history_new() {
  struct history *h = malloc(sizeof(struct history));
  memset(h, 0, sizeof(struct history));
  return h;
}

static int history_change(struct history *h, float x, float y) {
  if (!FLOAT_EQ(h->xs[h->i], x) || !FLOAT_EQ(h->ys[h->i], y)) {
    h->count = MIN(HISTORY_LEN, h->count + 1);
    h->xs[h->i] = x;
    h->ys[h->i] = y;
    h->i++;
    h->i %= HISTORY_LEN;
    return 1;
  }
  return 0;
}

static int coord_legit(float xy) {
  return IN_RANGE(-5000, xy, 5000) && !FLOAT_EQ(xy, 0) && !is_div_by(xy, 0.000250);
}

static int history_legit(struct history *h) {
  if (h->count != HISTORY_LEN) {
    return 0;
  }
  float total = 0, total_x = 0, total_y = 0;
  for (int i = h->i;; i = (i + 1) % HISTORY_LEN) {
    int j = (i + 1) % HISTORY_LEN;
    if (j == h->i) {
      break;
    }
    float d = dist(h->xs[i], h->ys[i], h->xs[j], h->ys[j]);
    total += d;
    if (d > MAX_SPEED / TICKS_PER_SEC) {
      return 0;
    }
    if (!coord_legit(h->xs[i]) || !coord_legit(h->ys[i])) {
      return 0;
    }
  }
  return total > (HISTORY_LEN / 10.0) * (MAX_SPEED / TICKS_PER_SEC) / 10;
}

static const int OFFSET_X = 66;
static int SAMPLE_PATTERN_X[] =
  {128, 63, 2, -1, 3, 128, -1, -1, -1, -1, 13}; // add -1 -1 to make div by 4?

static int OFFSET_Ps = -800;
static int SAMPLE_PATTERN_Ps[] =
  {97, -1, -1, 252, -1, -1, -1, -1, 208, -1, -1, -1, -1, -1, -1, -1, 64, 112, 18};

pthread_mutex_t LOCK;
hashtable *MY_XS;
hashtable *PS_XS;

static void *run_bg_scans(void *) {
  OPEN_MEM("cs2$"); // reboot on game restart... same for output thread
  for (;;) {
    READ_DS(BLOCKS);
    for (int i = 100; i < 180; i++) {
      mem_desc desc = ds[i];
      mem_block *block = read_mem_block(fd, desc.start, desc.size);
      char *bytes = block->bytes;
      for (size_t j = 0; j < block->size; j++) {
        uintptr_t x_addr = desc.start + j + OFFSET_X;
        if (matches(bytes + j, SAMPLE_PATTERN_X, SIZEARR(SAMPLE_PATTERN_X))
            && !hash_hask(MY_XS, KV(.uint64 = x_addr))) {
          hash_set(MY_XS, KV(.uint64 = x_addr), KV(.ptr = history_new()));
        }
        uintptr_t p_addr = desc.start + j + OFFSET_Ps;
        if (matches(bytes + j, SAMPLE_PATTERN_Ps, SIZEARR(SAMPLE_PATTERN_Ps))
            && !hash_hask(PS_XS, KV(.uint64 = p_addr))) {
          hash_set(PS_XS, KV(.uint64 = p_addr), KV(.ptr = history_new()));
        }
      }
      free_mem(block);
    }
    sleep(30);
  }
}

static void print(int fd, hashtable *tbl) {
  float dedup[4096];
  int d = 0, j = 0;
  size_t len = tbl->len;
  kv *x_keys = hash_keys(tbl);
  len = tbl->len;
  //printf("LEN: %ld\n", len);
  for (int i = 0; i < len; i++) {
    struct history *h = hash_getv(tbl, x_keys[i]).ptr;
    float curr_x = read_mem_word32(fd, x_keys[i].uint64).float32;
    float curr_y = read_mem_word32(fd, x_keys[i].uint64 + 4).float32;
    if (history_change(h, curr_x, curr_y) && history_legit(h)) {
      for (j = 0; j < d; j += 2) {
        if (dist(curr_x, curr_y, dedup[j], dedup[j + 1]) < 30.0) {
          break;
        }
      }
      if (j == d) {
        printf("(%.02f,%.02f) ", curr_x, curr_y);
        dedup[d++] = curr_x;
        dedup[d++] = curr_y;
      }
      if (d >= SIZEARR(dedup)) {
        err_fatal("too many coords!");
      }
    }
  }
  puts("");
  fflush(stdout);
}

static void *run_bg_output(void *) {
  OPEN_MEM("cs2$");
  for (;;) {
    //printf("\e[1;1H\e[2J");
    print(fd, MY_XS);
    print(fd, PS_XS);
    usleep(1000 / TICKS_PER_SEC * 1000);
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
