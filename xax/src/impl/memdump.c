#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include "args.h"
#include "proc.h"
#include "util.h"
#include "impl.h"
#include "hashtable.h"
#include "scan.h"

#define ADDR_RANGE 2 * 1024
#define ADDR_LEN (2 * ADDR_RANGE + 1)
#define BYTE_STR_LEN (3 * ADDR_LEN + 2)

static char FILEBUF[4096 * ADDR_LEN + 1];
static char *LINE_STRS[4096], *BYTE_STRS[4096][ADDR_LEN];
static uint8_t BYTE_SAMPLE[ADDR_LEN];
static char BYTE_STR[BYTE_STR_LEN];

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

static void dump(char *filename, uint8_t *bytes) {
  int fd = open(filename, O_WRONLY | O_CREAT | O_APPEND, 0666);
  if (fd <= 0) {
    err_fatal("open");
  }
  int len = 0;
//  len += snprintf(FILEBUF + len, 128, "[%s] [0x%lx] [0x%lx-0x%lx]\n", tag, addr, addr - ADDR_RANGE, addr + ADDR_RANGE);
  for (int i = 0; i < ADDR_LEN; i++) {
    len += snprintf(FILEBUF + len, 5, "%02X ", bytes[i]);
  }
  len += snprintf(FILEBUF + len, 2, "\n");
  ssize_t total = 0;
  do {
    ssize_t written = write(fd, FILEBUF + total, len - total);
    if (written <= 0) {
      err_fatal("write");
    }
    total += written;
  } while (total < len);
  printf("Written %zu bytes\n", total);
  close(fd);
}

static void memdump(void) {
  char *proc_name = args_get("arg0");
  char *addr_str = args_get("arg1");
  char *end = NULL;
  uintptr_t addr = strtoull(addr_str, &end, 16);
  if (*end != 0) {
    err_fatal("bad addr");
  }
  OPEN_MEM(proc_name);
  uint8_t bytes[ADDR_LEN];
  if (read_mem_bytes(fd, addr - ADDR_RANGE, bytes, SIZEARR(bytes)) != ADDR_LEN) {
    err_fatal("read_mem_bytes");
  }
  dump("bytes.txt", bytes);
  close_mem(fd);
}

static void compare(void) {
  int fd = open("bytes.txt", O_RDONLY);
  if (fd <= 0) {
    err_fatal("read");
  }
  ssize_t size = read(fd, FILEBUF, SIZEARR(FILEBUF));
  FILEBUF[size] = 0;
  size_t lines = strsplit(FILEBUF, "\n", LINE_STRS, 10);
  for (int i = 0; i < lines; i++) {
    size_t line_len = strsplit(LINE_STRS[i], " ", BYTE_STRS[i], ADDR_LEN);
    printf("Line %d with %zu bytes\n", i, line_len);
  }
  for (int i = 0; i < ADDR_LEN; i++) {
    int j;
    for (j = 0; j < lines - 1; j++) {
      if (!strcmp(BYTE_STRS[j][i], "00") || strcmp(BYTE_STRS[j][i], BYTE_STRS[j + 1][i])) {
        break;
      }
    }
    if (j == lines - 1) {
      printf("%d ", i);
    }
  }
  printf("\n");
}

static void pattern(void) {
  char *indexes_str = args_get("arg0");
  char *indexes_strs[512];
  int fd = open("bytes.txt", O_RDONLY);
  if (fd <= 0) {
    err_fatal("read");
  }
  ssize_t size = read(fd, FILEBUF, SIZEARR(FILEBUF));
  FILEBUF[size] = 0;
  size_t lines = strsplit(FILEBUF, "\n", LINE_STRS, 10);
  strsplit(LINE_STRS[0], " ", BYTE_STRS[0], ADDR_LEN);
  size_t ixs = strsplit(indexes_str, " ", indexes_strs, SIZEARR(indexes_strs));
  for (int i = 0; i < ixs; i++) {
    int ix = (int)strtol(indexes_strs[i], NULL, 10);
    int byte = (uint8_t)strtol(BYTE_STRS[0][ix], NULL, 16);
    printf("%d, ", byte);
    if (i < ixs - 1) {
      int ix2 = (int)strtol(indexes_strs[i + 1], NULL, 10);
      for (int j = 0; j < ix2 - ix - 1; j++) {
        printf("-1, ");
      }
    }
  }
  printf("\n");
}

static int sample_addr(int fd, uintptr_t addr, size_t range, uint8_t buf[]) {
  size_t r = read_mem_bytes(fd, addr - range, (char *)buf, 2 * range + 1);
  return r == 2 * range + 1;
}

static void samples_init(pid_t pid, int fd, hashtable *tbl, float target) {
  FOREACH_BLOCK(1, 20, {
    SCAN(block, {
      if (IN_RANGE(target - 0.01, word.float32, target + 0.01)) {
        hash_set(tbl, KV(.uint64 = WORD_ADDR), KV(.float32 = word.float32));
      }
    });
  });
}

static void samples_refine(int fd, hashtable *tbl, float target) {
  FOREACH_KV(tbl, {
    union word32 word;
    read_mem_bytes(fd, key.uint64, word.bytes, 4);
    if (!IN_RANGE(target - 0.01, word.float32, target + 0.01)) {
      hash_del(tbl, key);
    }
  });
}

static void sampler(void) {
  char *proc_name = args_get("arg0");
  char *file_name = args_get("arg1");
  char input[128];
  char *tokens[8];
  float target;
  hashtable *tbl = NULL;
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
      if (tbl != NULL) {
        hash_free(tbl);
        tbl = NULL;
      }
    } else if (strcasestr("scan", tokens[0])) {
      if (tbl == NULL) {
        // initial scan
        if (!parsef(tokens[1], &target)) {
          puts("bad float");
          continue;
        }
        tbl = hash_new(128 * 128, hash_int, hash_cmp_int);
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
      int file = open(file_name, O_WRONLY | O_CREAT | O_APPEND, 0666);
      FOREACH_KV(tbl, {
        if (!sample_addr(fd, key.uint64, ADDR_RANGE, BYTE_SAMPLE)) {
          err_fatal("sample_addr");
        }
        byteline(BYTE_STR, BYTE_SAMPLE, ADDR_LEN);
        if (write_all(file, BYTE_STR, BYTE_STR_LEN - 1) < BYTE_STR_LEN
            || write(file, "\n", 1) <= 0) {
          puts("error writing");
        }
      });
      close(file);
      printf("samples dumped to [%s]\n", file_name);
      break;
    } else {
      printf("unknown command [%s]\n", tokens[0]);
    }
  }
  if (tbl != NULL) {
    hash_free(tbl);
  }
}

__attribute__((constructor))
static void init(void) {
  args_add("memdump", memdump);
  args_add("memcmp", compare);
  args_add("pattern", pattern);
  args_add("sampler", sampler);
}