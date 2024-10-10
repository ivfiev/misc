#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include "args.h"
#include "scan.h"
#include "util.h"
#include "proc.h"

#define OPEN_MEM(proc_name) \
  pid_t pid = get_pid(proc_name); \
  if (pid <= 0) { \
    err_fatal("pid"); \
  } \
  int fd = open_mem(pid)   \

#define PARSE_RANGE() \
  char from_str[16], to_str[16]; \
  if (sscanf(range_str, "(%15[^,],%15[^)])", from_str, to_str) != 2) { \
    err_fatal("bad range32 input"); \
  } \
  float from = parse_value(from_str, FLOAT32_TYPE).float32; \
  float to = parse_value(to_str, FLOAT32_TYPE).float32                \

#define READ_DS(size) \
  mem_desc ds[size]; \
  size_t ds_size = read_mem_desc(pid, ds, SIZEARR(ds))


static int use_stdin(void) {
  return !strcmp("--stdin", args_get("arg2"));
}

void set32(void) {
  char *proc_name = args_get("arg0");
  char *addr_str = args_get("arg1");
  char *value_str = args_get("arg2");
  OPEN_MEM(proc_name);
  uintptr_t addr = parse_addr(addr_str);
  union word64 value = parse_value(value_str, INFER_TYPE);
  write_mem(fd, addr, value.bytes, 4);
  close_mem(fd);
}

void range32(void) {
  char *proc_name = args_get("arg0");
  char *range_str = args_get("arg1");
  OPEN_MEM(proc_name);
  PARSE_RANGE();
  FOREACH_BLOCK({
    SCAN(block, {
      if (IN_RANGE(from, (float)word.int32, to)) {
        printf("%d 0x%lx 0x%lx %d\n", i, WORD_ADDR, offset, word.int32);
      } else if (IN_RANGE(from, word.float32, to)) {
        printf("%d 0x%lx 0x%lx %f\n", i, WORD_ADDR, offset, word.float32);
      }
    });
  });
  close_mem(fd);
}

void delta32(void) {
  char *proc_name = args_get("arg0");
  char *range_str = args_get("arg1");
  OPEN_MEM(proc_name);
  PARSE_RANGE();
  char line[128];
  while ((fgets(line, SIZEARR(line), stdin) != NULL)) {
    union word64 word;
    int ix;
    uintptr_t word_addr, offset;
    float prev;
    if (sscanf(line, "%d %lx %lx %f", &ix, &word_addr, &offset, &prev) == 4) {
      read_mem_bytes(fd, word_addr, word.bytes, 4);
      if (word.float32 == prev) {
        continue;
      }
      if (IN_RANGE(prev + from, (float)word.int32, prev + to)) {
        printf("%d 0x%lx 0x%lx %d\n", ix, word_addr, offset, word.int32);
      } else if (IN_RANGE(prev + from, word.float32, prev + to)) {
        printf("%d 0x%lx 0x%lx %f\n", ix, word_addr, offset, word.float32);
      }
    } else {
      printf("bad input: %s\n", line);
    }
  }
  close_mem(fd);
}

void info32(void) {
  char *proc_name = args_get("arg0");
  char *addr_str = args_get("arg1");
  char *range_str = args_get("arg2");
  OPEN_MEM(proc_name);
  PARSE_RANGE();
  uintptr_t addr = parse_addr(addr_str);
  if (addr % 4 != 0) {
    err_fatal("bad addr");
  }
  READ_DS(1536);
  for (uintptr_t ptr = addr + from * 4; ptr <= addr + to * 4; ptr += 4) {
    int off = 0;
    char line[128];
    union word64 word = {.int64 = 0};
    read_mem_bytes(fd, ptr, word.bytes, 4);
    off += snprintf(line, SIZEARR(line), "0x%lx  ", ptr);
    if (is_int32(word)) {
      off += snprintf(line + off, SIZEARR(line) - off, "Integer %d", word.int32);
    } else if (is_float32(word)) {
      off += snprintf(line + off, SIZEARR(line) - off, "Float32 %f", word.float32);
    } else if (is_ptr(word, ds, ds_size)) {
      off += snprintf(line + off, SIZEARR(line) - off, "Pointer 0x%lx", word.ptr64);
    } else {
      off += snprintf(line + off, SIZEARR(line) - off, "Unknown %lx", word.ptr64);
    }
    if (ptr == addr) {
      snprintf(line + off, SIZEARR(line) - off, "  -----");
    }
    printf("%s\n", line);
  }
  close_mem(fd);
}

void ptr_scan32(void) {

}

__attribute__((constructor))
static void init(void) {
  args_add("set32", set32);
  args_add("range32", range32);
  args_add("delta32", delta32);
  args_add("info32", info32);
}