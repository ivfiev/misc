#ifndef H3SO_H
#define H3SO_H

#include "stdint.h"
#include <cstddef>

struct binary {
  uint8_t bytes[16];
  size_t size;
};

binary patch_rx(uintptr_t ptr, binary code);
binary get_jmp_code(uintptr_t from, uintptr_t to, size_t size);

void log(const char *format, ...);
void inject();
void unject();

#endif // H3SO_H