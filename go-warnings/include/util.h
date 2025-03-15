#ifndef GO_UTIL_H
#define GO_UTIL_H

#include <stdint.h>
#include <sys/types.h>

#define WORD_SIZE (sizeof(long))

ssize_t ptrace_read(pid_t pid, void *addr, uint8_t buf[], size_t count);

ssize_t ptrace_write(pid_t pid, void *addr, uint8_t buf[], size_t count);

#endif