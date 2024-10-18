#ifndef XAX_MEM_H
#define XAX_MEM_H

#include <stdint.h>

#define ADDR_RANGE 2 * 1024
#define ADDR_LEN (2 * ADDR_RANGE + 1)
#define BYTE_STR_LEN (3 * ADDR_LEN + 2)

static char FILEBUF[4096 * ADDR_LEN + 1];
static char *LINE_STRS[4096], *BYTE_STRS[4096][ADDR_LEN];
static uint8_t BYTE_SAMPLE[ADDR_LEN];
static char BYTE_STR[BYTE_STR_LEN];

#endif
