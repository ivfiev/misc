#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include "args.h"
#include "scan.h"
#include "util.h"
#include "proc.h"

#define OPEN_MEM(proc_name) \
  pid_t pid= get_pid(proc_name); \
  if (pid <= 0) { \
    err_fatal("pid"); \
  } \
  int fd = open_mem(pid); \


static int use_stdin(void) {
  return !strcmp("--stdin", args_get("arg2"));
}

// TODO simplify, get rid of 'equal' & don't parse integers
static union word64 parse_value(char *val_str) {
  char *end = NULL;
  union word64 ret;
  if (strstr(val_str, ".")) {
    ret.float32 = (float)strtod(val_str, &end);
  } else {
    ret.int32 = (int)strtol(val_str, &end, 10);
  }
  if (*end != 0) {
    err_fatal("parse_value");
  }
  return ret;
}

static float parse_float(char *val_str) {
  float ret;
  char *end = NULL;
  ret = (float)strtod(val_str, &end);
  if (*end != 0) {
    err_fatal("parse_float");
  }
  return ret;
}

static uintptr_t parse_addr(char *addr_str) {
  char *end = NULL;
  uintptr_t addr = strtoull(addr_str, &end, 16);
  if (*end != 0) {
    err_fatal("parse_addr");
  }
  return addr;
}

static void equal32_full_scan(union word64 needle, int fd, mem_desc ds[], size_t ds_size) {
  for (int i = 0; i < ds_size; i++) {
    mem_desc desc = ds[i];
    mem_block *block = read_mem_block(fd, desc.start, desc.size);
    SCAN(block, {
      if (word.int32 == needle.int32) {
        printf("%d 0x%lx 0x%lx\n", i, WORD_ADDR, offset);
      }
    });
  }
}

static void equal32_stdin_scan(union word64 needle, int fd) {
  char line[128];
  while ((fgets(line, SIZEARR(line), stdin) != NULL)) {
    union word64 word;
    int ix;
    uintptr_t word_addr, offset;
    if (sscanf(line, "%d %lx %lx", &ix, &word_addr, &offset) == 3) {
      read_mem_bytes(fd, word_addr, word.bytes, 4);
      if (word.int32 == needle.int32) {
        printf("%d 0x%lx 0x%lx\n", ix, word_addr, offset);
      }
    } else {
      printf("bad input: %s\n", line);
    }
  }
}

void equal32(void) {
  char *proc_name = args_get("arg0");
  char *value_str = args_get("arg1");
  union word64 needle = parse_value(value_str);
  OPEN_MEM(proc_name);
  if (use_stdin()) {
    equal32_stdin_scan(needle, fd);
  } else {
    mem_desc ds[1024];
    size_t ds_size = read_mem_desc(pid, ds, SIZEARR(ds));
    equal32_full_scan(needle, fd, ds, ds_size);
  }
}

void set32(void) {
  char *proc_name = args_get("arg0");
  char *addr_str = args_get("arg1");
  char *value_str = args_get("arg2");
  OPEN_MEM(proc_name);
  uintptr_t addr = parse_addr(addr_str);
  union word64 value = parse_value(value_str);
  write_mem(fd, addr, value.bytes, 4);
}

void range32(void) {
  char *proc_name = args_get("arg0");
  char *range_str = args_get("arg1");
  OPEN_MEM(proc_name);
  char from_str[16], to_str[16];
  sscanf(range_str, "(%15[^,],%15[^)])", from_str, to_str);
  float from = parse_float(from_str);
  float to = parse_float(to_str);
  mem_desc ds[1024];
  size_t ds_size = read_mem_desc(pid, ds, SIZEARR(ds));
  for (int i = 0; i < ds_size; i++) {
    mem_desc desc = ds[i];
    mem_block *block = read_mem_block(fd, desc.start, desc.size);
    SCAN(block, {
      if (IN_RANGE(from, word.int32, to)) {
        printf("%d 0x%lx 0x%lx %d\n", i, WORD_ADDR, offset, word.int32);
      } else if (IN_RANGE(from, word.float32, to)) {
        printf("%d 0x%lx 0x%lx %f\n", i, WORD_ADDR, offset, word.float32);
      }
    });
  }
}

__attribute__((constructor))
static void init(void) {
  args_add("equal32", equal32);
  args_add("set32", set32);
  args_add("range32", range32);
}