#include <stdio.h>
#include <unistd.h>
#include "args.h"
#include "proc.h"
#include "util.h"
#include "impl.h"
#include "hashtable.h"
#include <pthread.h>
#include <string.h>
#include "csrh.h"

static const int OFFSET_X = 66;
static int SAMPLE_PATTERN_X[] =
  {128, 63, 2, -1, 3, 128, -1, -1, -1, -1, 13}; // add -1 -1 to make div by 4?

static int OFFSET_Ps = -344;
static int SAMPLE_PATTERN_Ps[] =
  {16, 22, 152, -1, -1, -1, -1, -1, 176, 176, 146};

pthread_mutex_t LOCK;
hashtable *MY_XS;
hashtable *PS_XS;

static void *run_bg_scans(void *) {
  OPEN_MEM("cs2$");
  int min = 100, max = 180;
  for (;;) {
    READ_DS(BLOCKS);
    fprintf(stderr, "starting a scan over [%d, %d]\n", min, max);
    int scan_min = BLOCKS, scan_max = -1;
    for (int i = min; i < max; i++) {
      mem_desc desc = ds[i];
      mem_block *block = read_mem_block(fd, desc.start, desc.size);
      char *bytes = block->bytes;
      for (size_t j = 0; j < block->size; j++) {
        uintptr_t x_addr = desc.start + j + OFFSET_X;
        if (matches(bytes + j, SAMPLE_PATTERN_X, SIZEARR(SAMPLE_PATTERN_X))
            && !hash_hask(MY_XS, KV(.uint64 = x_addr))) {
          hash_set(MY_XS, KV(.uint64 = x_addr), KV(.ptr = history_new()));
          scan_min = MIN(scan_min, i - 10);
          scan_max = MAX(scan_max, i + 10);
        }
        uintptr_t p_addr = desc.start + j + OFFSET_Ps;
        if (matches(bytes + j, SAMPLE_PATTERN_Ps, SIZEARR(SAMPLE_PATTERN_Ps))
            && !hash_hask(PS_XS, KV(.uint64 = p_addr))) {
          hash_set(PS_XS, KV(.uint64 = p_addr), KV(.ptr = history_new()));
          scan_min = MIN(scan_min, i - 10);
          scan_max = MAX(scan_max, i + 10);
        }
      }
      free_mem(block);
    }
    min = scan_min < BLOCKS ? scan_min : min;
    max = scan_max > 0 ? scan_max : max;
    fprintf(stderr, "scan complete [%d, %d]\n", min, max);
    fprintf(stderr, "#stored addresses (%zu, %zu)\n", MY_XS->len, PS_XS->len);
    sleep(30);
  }
}

static void print(int fd, hashtable *tbl, char *prefix) {
  double votes[1024];
  int j = 0, v = 0;
  size_t len = tbl->len;
  kv *x_keys = hash_keys(tbl);
  len = tbl->len;
  for (int i = 0; i < len; i++) {
    struct history *h = hash_getv(tbl, x_keys[i]).ptr;
    float curr_x = read_mem_word32(fd, x_keys[i].uint64).float32;
    float curr_y = read_mem_word32(fd, x_keys[i].uint64 + 4).float32;
    if (history_change(h, curr_x, curr_y) && history_legit(h)) {
      for (j = 0; j < v; j += 3) {
        if (dist(curr_x, curr_y, votes[j], votes[j + 1]) < 30.0) {
          votes[j] = (votes[j + 2] * votes[j] + curr_x) / (votes[j + 2] + 1);
          votes[j + 1] = (votes[j + 2] * votes[j + 1] + curr_y) / (votes[j + 2] + 1);
          votes[j + 2]++;
          break;
        }
      }
      if (j == v) {
        votes[v++] = curr_x;
        votes[v++] = curr_y;
        votes[v++] = 1;
      }
      if (v >= SIZEARR(votes)) {
        err_fatal("too many coords!");
      }
    }
  }
  for (int i = 0; i < v; i += 3) {
    printf("%s(%.02f,%.02f)\n", (prefix != NULL ? prefix : ""), votes[i], votes[i + 1]);
  }
  free(x_keys);
  fflush(stdout);
}

static void *run_bg_output(void *) {
  OPEN_MEM("cs2$");
  for (;;) {
    print(fd, MY_XS, "ME");
    print(fd, PS_XS, NULL);
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

// ts + legit flag
// keys & pynput
// del old addrs (lock)
// refresh pid