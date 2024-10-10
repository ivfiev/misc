#include "scan.h"
#include "util.h"

union word64 parse_value(char *val_str, int type) {
  char *end = NULL;
  union word64 ret;
  if (type == INFER_TYPE) {
    if (strstr(val_str, ".") != NULL) {
      ret.float32 = (float)strtod(val_str, &end);
    } else {
      ret.int32 = (int)strtol(val_str, &end, 10);
    }
  } else {
    if (type == FLOAT32_TYPE) {
      ret.float32 = (float)strtod(val_str, &end);
    }
  }
  if (end == NULL || *end != 0) {
    err_fatal("parse_value");
  }
  return ret;
}

uintptr_t parse_addr(char *addr_str) {
  char *end = NULL;
  uintptr_t addr = strtoull(addr_str, &end, 16);
  if (*end != 0) {
    err_fatal("parse_addr");
  }
  return addr;
}