#include "scan.h"

int ile(uint w) {
  return ((w & 0xFF) << 24) |    // NOLINT(*-narrowing-conversions)
         ((w & 0xFF00) << 8) |
         ((w & 0xFF0000) >> 8) |
         ((w & 0xFF000000) >> 24);
}

float fle(uint w) {
  u_int8_t *bytes = (u_int8_t *)&w;
  for (int i = 0; i < 2; i++) {
    bytes[i] ^= bytes[3 - i] ^= bytes[i] ^= bytes[3 - i];
  }
  return *(float *)bytes;
}
