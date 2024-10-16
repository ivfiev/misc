#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include "args.h"
#include "proc.h"
#include "util.h"
#include "impl.h"

#define ADDR_RANGE 2 * 1024
#define ADDR_LEN (2 * ADDR_RANGE + 1)
#define FILENAME "bytes.txt"

static char filebuf[50 * ADDR_LEN + 1];
static char *line_strs[10], *byte_strs[10][ADDR_LEN];

static void dump(uint8_t *bytes, uintptr_t addr) {
  int fd = open(FILENAME, O_WRONLY | O_CREAT | O_APPEND, 0666);
  if (fd <= 0) {
    err_fatal("open");
  }
  int len = 0;
//  len += snprintf(filebuf + len, 128, "[%s] [0x%lx] [0x%lx-0x%lx]\n", tag, addr, addr - ADDR_RANGE, addr + ADDR_RANGE);
  for (int i = 0; i < ADDR_LEN; i++) {
    len += snprintf(filebuf + len, 5, "%02X ", bytes[i]);
  }
  len += snprintf(filebuf + len, 2, "\n");
  ssize_t total = 0;
  do {
    ssize_t written = write(fd, filebuf + total, len - total);
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
  dump(bytes, addr);
  close_mem(fd);
}

static void compare(void) {
  int fd = open(FILENAME, O_RDONLY);
  if (fd <= 0) {
    err_fatal("read");
  }
  ssize_t size = read(fd, filebuf, SIZEARR(filebuf));
  filebuf[size] = 0;
  size_t lines = strsplit(filebuf, "\n", line_strs, 10);
  for (int i = 0; i < lines; i++) {
    size_t line_len = strsplit(line_strs[i], " ", byte_strs[i], ADDR_LEN);
    printf("Line %d with %zu bytes\n", i, line_len);
  }
  for (int i = 0; i < ADDR_LEN; i++) {
    int j;
    for (j = 0; j < lines - 1; j++) {
      if (!strcmp(byte_strs[j][i], "00") || strcmp(byte_strs[j][i], byte_strs[j + 1][i])) {
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
  int fd = open(FILENAME, O_RDONLY);
  if (fd <= 0) {
    err_fatal("read");
  }
  ssize_t size = read(fd, filebuf, SIZEARR(filebuf));
  filebuf[size] = 0;
  size_t lines = strsplit(filebuf, "\n", line_strs, 10);
  strsplit(line_strs[0], " ", byte_strs[0], ADDR_LEN);
  size_t ixs = strsplit(indexes_str, " ", indexes_strs, SIZEARR(indexes_strs));
  for (int i = 0; i < ixs; i++) {
    int ix = (int)strtol(indexes_strs[i], NULL, 10);
    int byte = (uint8_t)strtol(byte_strs[0][ix], NULL, 16);
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

__attribute__((constructor))
static void init(void) {
  args_add("memdump", memdump);
  args_add("memcmp", compare);
  args_add("pattern", pattern);
}