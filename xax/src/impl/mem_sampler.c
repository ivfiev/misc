#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include "args.h"
#include "proc.h"
#include "util.h"
#include "impl.h"
#include "hashtable.h"
#include "scan.h"
#include "mem.h"

#define PRECISION 0.01

static int parsef(char *str, float *f) {
  char *endptr = NULL;
  *f = strtof(str, &endptr);
  return *endptr == 0;
}

static size_t byteline(char buf[], uint8_t *bytes, size_t bytes_size) {
  size_t written = 0;
  for (int i = 0; i < bytes_size && written < 3 * bytes_size + 1; i++) {
    written += snprintf(buf + written, 5, "%02X ", bytes[i]);
  }
  return written;
}

static int sample_addr(int fd, uintptr_t addr, size_t range, uint8_t buf[]) {
  size_t r = read_mem_bytes(fd, addr - range, (char *)buf, 2 * range + 1);
  return r == 2 * range + 1;
}

static void samples_init(pid_t pid, int fd, hashtable *tbl, float target) {
  FOREACH_BLOCK(1, 3000, {
    SCAN(block, {
      if (IN_RANGE(target - PRECISION, word.float32, target + PRECISION)) {
        hash_set(tbl, KV(.uint64 = WORD_ADDR), KV(.float32 = word.float32));
      }
    });
  });
}

static void samples_refine(int fd, hashtable *tbl, float target) {
  FOREACH_KV(tbl, {
    union word32 word;
    read_mem_bytes(fd, key.uint64, word.bytes, 4);
    if (!IN_RANGE(target - PRECISION, word.float32, target + PRECISION)) {
      hash_del(tbl, key);
    }
  });
}

static void samples_sieve(int fd, hashtable *tbl) {
  int count = 0;
  union word32 test = {.float32 = 888.888};
  union word32 control, original;
  FOREACH_KV(tbl, {
    read_mem_bytes(fd, key.uint64, original.bytes, 4);
    write_mem(fd, key.uint64, test.bytes, 4);
    usleep(80 * 1000);
    int changed = 0;
    for (int i = 0; i < tbl_len; i++) {
      if (kvs[i].uint64 == key.uint64) {
        continue;
      }
      read_mem_bytes(fd, kvs[i].uint64, control.bytes, 4);
      if (FLOAT_EQ(control.float32, test.float32)) {
        changed++;
      }
    }
    write_mem(fd, key.uint64, original.bytes, 4);
    if ((double)changed / (double)tbl_len > 0.05) {
      count++;
      printf("0x%lx\n", key.uint64);
    } else {
      hash_del(tbl, key);
    }
    usleep(20 * 1000);
  });
}

static void samples_dump(char *filename, int mem_fd, hashtable *tbl) {
  uint8_t byte_sample[SAMPLE_SIZE];
  char byte_str[BYTE_STR_LEN];
  int file_fd = open(filename, O_WRONLY | O_CREAT | O_APPEND, 0666);
  FOREACH_KV(tbl, {
    if (!sample_addr(mem_fd, key.uint64, SAMPLE_RADIUS, byte_sample)) {
      fprintf(stderr, "error reading from [%lx]\n", key.uint64);
    }
    byteline(byte_str, byte_sample, SAMPLE_SIZE);
    if (write_all(file_fd, byte_str, BYTE_STR_LEN - 1) < BYTE_STR_LEN - 1
        || write(file_fd, "\n", 1) <= 0) {
      fprintf(stderr, "error writing [%s]\n", strerror(errno));
    }
  });
  close(file_fd);
}

static void samples_ptrdfs(pid_t pid, int mem_fd, hashtable *tbl) {
  const int byte_dist = 1024;
  mem_block *bs[256];
  read_mem_blocks(pid, mem_fd, bs, SIZEARR(bs));
  FOREACH_KV(tbl, {
    uintptr_t val_addr = key.uint64;
    hashtable *visited = hash_new(256, hash_int, hash_cmp_int);
    PTR_SCAN_START:
    if (!hash_getv(visited, KV(.uint64 = val_addr)).int32) {
      hash_set(visited, KV(.uint64 = val_addr), KV(.int32 = 1));
      for (int i = 0; i <= byte_dist / 4; i++) {
        uintptr_t addr = val_addr - i * 4;
        for (int j = 0; j < SIZEARR(bs); j++) {
          SCAN(bs[j], {
            if (word.ptr64 == addr) {
              printf("[0x%lx + 0x%x] <- [0x%lx] (0x%lx - 0x%lx = |0x%lx|)\n",
                addr, i * 4, WORD_ADDR, addr, WORD_ADDR, MAX(addr, WORD_ADDR) - MIN(addr, WORD_ADDR));
              val_addr = WORD_ADDR;
              goto PTR_SCAN_START;
            }
          });
        }
      }
    }
    puts("---");
    hash_free(visited);
  });
  for (int i = 0; i < SIZEARR(bs); i++) {
    free_mem(bs[i]);
  }
}

static void samples_ptrbfs(pid_t pid, int mem_fd, hashtable *tbl) {
  const int byte_dist = 1024;
  mem_block *bs[256];
  read_mem_blocks(pid, mem_fd, bs, SIZEARR(bs));
  FOREACH_KV(tbl, {
    uintptr_t val_addr = key.uint64;
    for (int i = 0; i <= byte_dist / 4; i++) {
      uintptr_t addr = val_addr - i * 4;
      for (int j = 0; j < SIZEARR(bs); j++) {
        SCAN(bs[j], {
          if (word.ptr64 == addr) {
            printf("[0x%lx + 0x%x] <- [0x%lx] (0x%lx - 0x%lx = |0x%lx|)\n",
              addr, i * 4, WORD_ADDR, addr, WORD_ADDR, MAX(addr, WORD_ADDR) - MIN(addr, WORD_ADDR));
          }
        });
      }
    }
    puts("---");
  });
  for (int i = 0; i < SIZEARR(bs); i++) {
    free_mem(bs[i]);
  }
}

static void samples_add(int mem_fd, hashtable *tbl, uintptr_t addr) {
  union word32 word;
  read_mem_bytes(mem_fd, addr, word.bytes, 4);
  hash_set(tbl, KV(.uint64 = addr), KV(.float32 = word.float32));
}

static void sampler(void) {
  char *proc_name = args_get("arg0");
  char *filename = args_get("arg1");
  char input[128];
  char *tokens[8];
  float target;
  hashtable *tbl = hash_new(128 * 128, hash_int, hash_cmp_int);
  OPEN_MEM(proc_name);
  for (;;) {
    printf("> ");
    fflush(stdin);
    if (!fgets(input, SIZEARR(input), stdin)) {
      break;
    }
    trim_end(input);
    strsplit(input, " ", tokens, SIZEARR(tokens));
    if (strcasestr("reset", tokens[0])) {
      // cleanup & reset session
      hash_free(tbl);
      tbl = hash_new(128 * 128, hash_int, hash_cmp_int);
    } else if (strcasestr("scan", tokens[0])) {
      if (tbl->len == 0) {
        // initial scan
        if (!parsef(tokens[1], &target)) {
          puts("bad float");
          continue;
        }
        samples_init(pid, fd, tbl, target);
        printf("Address count [%zu]\n", tbl->len);
      } else {
        // refining scan
        if (!parsef(tokens[1], &target)) {
          puts("bad input");
          continue;
        }
        samples_refine(fd, tbl, target);
        printf("Address count [%zu]\n", tbl->len);
      }
    } else if (strcasestr("dump", tokens[0])) {
      // sample & dump to filename
      samples_dump(filename, fd, tbl);
      printf("Samples dumped to [%s]\n", filename);
      break;
    } else if (strcasestr("sieve", tokens[0])) {
      samples_sieve(fd, tbl);
      printf("Address count: [%zu]\n", tbl->len);
    } else if (strcasestr("ptrbfs", tokens[0])) {
      samples_ptrbfs(pid, fd, tbl);
    } else if (strcasestr("ptrdfs", tokens[0])) {
      samples_ptrdfs(pid, fd, tbl);
    } else if (strcasestr("add", tokens[0])) {
      uintptr_t addr = parse_addr(tokens[1]);
      samples_add(fd, tbl, addr);
      printf("Address count: [%zu]\n", tbl->len);
    } else {
      printf("unknown command [%s]\n", tokens[0]);
    }
  }
  if (tbl != NULL) {
    hash_free(tbl);
    close_mem(fd);
  }
}

__attribute__((constructor))
static void init(void) {
  args_add("sampler", sampler);
}