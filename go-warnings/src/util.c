#include "util.h"
#include <sys/ptrace.h>
#include <string.h>

ssize_t ptrace_read(pid_t pid, void *addr, uint8_t buf[], size_t count) {
  size_t i;
  for (i = 0; i < count; i += WORD_SIZE) {
    long word = ptrace(PTRACE_PEEKDATA, pid, addr + i, 0);
    memcpy(buf + i, &word, WORD_SIZE);
  }
  return i >= count ? i : -1;
}

ssize_t ptrace_write(pid_t pid, void *addr, uint8_t buf[], size_t count) {
  return -1;
}