#include "h3so.hxx"
#include "stdint.h"
#include <unistd.h>
#include <thread>

typedef void (*award_xp_t)(uint32_t, uint32_t, uint32_t);

static award_xp_t AWARD_XP_FUNC = (award_xp_t)0x4e3620;

__attribute__((optimize("O0")))
void award_xp(uintptr_t hero, uint32_t xp, uint32_t arg0, uint32_t arg1) {
  asm("mov %0, %%ecx" : : "r"(hero) : "%ecx");
  AWARD_XP_FUNC(xp, arg0, arg1);
}

void award_thread() {
  for (;;) {
    sleep(5);
    award_xp(0x5501640, 2000, 0, 0);
  }
}

void inject() {
  std::thread {award_thread}.detach();
  log("injected");
}

void unject() {
}