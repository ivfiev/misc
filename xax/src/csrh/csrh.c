#include <stdio.h>
#include <unistd.h>
#include "args.h"
#include "proc.h"
#include "util.h"
#include "impl.h"
#include <string.h>
#include "math.h"

static uintptr_t OFFSETS_XYZ[] = {0x3948428, 0x688, 0x20, 0x344};
static uintptr_t OFFSETS_PITCH[] = {0x3782160, 0x184};

static uintptr_t hop(int mem_fd, uintptr_t base, uintptr_t offsets[], size_t size) {
  union word64 word = {.ptr64 = base};
  for (int i = 0; i < size - 1; i++) {
    word = read_mem_word64(mem_fd, word.ptr64 + offsets[i]);
  }
  return word.ptr64 + offsets[size - 1];
}

static void run(void) {
  OPEN_MEM("cs2$");
  READ_DS(1536);
  uintptr_t base = ds[find_mem_desc("libclient", ds, ds_size)].start;
  for (;;) {
    uintptr_t ptr_x = hop(fd, base, OFFSETS_XYZ, SIZEARR(OFFSETS_XYZ));
    float x = read_mem_word32(fd, ptr_x).float32;
    float y = read_mem_word32(fd, ptr_x + 4).float32;
    float z = read_mem_word32(fd, ptr_x + 8).float32;
    uintptr_t ptr_p = hop(fd, base, OFFSETS_PITCH, SIZEARR(OFFSETS_PITCH));
    float pitch_r = asin(read_mem_word32(fd, ptr_p).float32);
    float yaw_r = -atan2(read_mem_word32(fd, ptr_p - 16).float32, -read_mem_word32(fd, ptr_p - 32).float32);
    float pitch_d = pitch_r / M_PI_2 * 90;
    float yaw_d = yaw_r / M_PI * 180;
    printf("(%f, %f, %f) (%f, %f)\n", x, y, z, pitch_d, yaw_d);
    msleep(25);
  }
}

__attribute__((constructor))
static void init(void) {
  args_add("csrh", run);
}

// id t/ct
// use gtk/imgui or make tk bearable