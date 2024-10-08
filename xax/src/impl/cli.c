#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include "args.h"
#include "scan.h"
#include "util.h"
#include "proc.h"

static union word64 parse_value(char *val) {
  char *end = NULL;
  union word64 ret;
  if (strstr(val, ".")) {
    float f32 = (float)strtod(val, &end);
    ret.float32 = f32;
  } else {
    int i32 = (int)strtol(val, &end, 10);
    ret.int32 = i32;
  }
  if (*end != 0) {
    err_fatal("parse_value");
  }
  return ret;
}

void equal32(void) {
  char *proc_name = args_get("arg0");
  char *value_str = args_get("arg1");
  union word64 needle = parse_value(value_str);
  // macro this
  mem_desc ds[1024];
  pid_t pid = get_pid(proc_name);
  if (pid <= 0) {
    err_fatal("pid");
  }
  size_t ds_count = read_mem_desc(pid, ds, SIZEARR(ds));
  int fd = open_mem(pid);

  for (int i = 0; i < ds_count; i++) {
    mem_desc desc = ds[i];
    mem_block *block = read_mem(fd, desc.start, desc.size);
    SCAN(block, {
      if (word.int32 == needle.int32) {
        printf("%d %lx %lx\n", i, WORD_ADDR, offset);
      }
    });
  }
}

void set32(void) {

}

__attribute__((constructor))
static void init(void) {
  args_add("equal32", equal32);
}