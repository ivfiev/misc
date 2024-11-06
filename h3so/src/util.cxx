#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include "h3so.hxx"
#include <stdlib.h>
#include <sys/time.h>
#include <time.h>
#include <sys/mman.h>

#define LOG_FILE "/home/fi/dev/misc/h3so/h3sod.log"

void timestamp(char *buf) {
  struct timeval tv;
  struct tm *tm_info;
  gettimeofday(&tv, NULL);
  tm_info = localtime(&tv.tv_sec);
  size_t c = strftime(buf, 12, "%H:%M:%S", tm_info);
  snprintf(buf + c, 19, ".%03ld", tv.tv_usec / 1000);
}

void log(const char *format, ...) {
  char ts[32];
  FILE *log_file = fopen(LOG_FILE, "a+");
  if (log_file == nullptr) {
    exit(91);
  }
  timestamp(ts);
  va_list args;
  va_start(args, format);
  fprintf(log_file, "[%s] ", ts);
  vfprintf(log_file, format, args);
  fprintf(log_file, "\n");
  va_end(args);
  fclose(log_file);
}

binary patch_rx(uintptr_t ptr, binary code) {
  binary old;
  old.size = code.size;
  memcpy(old.bytes, (void *)ptr, old.size);
  if (mprotect((void *)(ptr - ptr % 0x1000), 0x1000, PROT_READ | PROT_WRITE | PROT_EXEC) != 0) {
    perror(strerror(errno));
  }
  memcpy((void *)ptr, code.bytes, code.size);
  if (mprotect((void *)(ptr - ptr % 0x1000), 0x1000, PROT_READ | PROT_EXEC) != 0) {
    perror(strerror(errno));
  }
  return old;
}

binary get_jmp_code(uintptr_t from, uintptr_t to, size_t size) {
  binary code;
  code.size = size;
  code.bytes[0] = 0xE9;
  *(uintptr_t *)(code.bytes + 1) = to - from - 5;
  for (int i = code.size; i < size; i++) {
    code.bytes[i] = 0x90;
  }
  return code;
}