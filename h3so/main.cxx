#include <stdio.h>
#include <stdint.h>
#include <sys/mman.h>
#include <string.h>
#include <unistd.h>
#include <exception>
#include <thread>
#include <signal.h>
#include <csignal>

typedef void (*award_xp_t)(uint32_t, uint32_t, uint8_t);

award_xp_t AWARD_XP_FUNC = (award_xp_t)0x4e3620;

__attribute__((optimize("O0")))
award_xp_t award_xp(uintptr_t hero, uint32_t xp, uint32_t arg0, uint8_t arg1) {
  asm("mov %0, %%ecx" : : "r"(hero) : "%ecx");
  AWARD_XP_FUNC(xp, arg0, arg1);
}

void donosink(int) {
// ignore signal for now
}

void award_thread() {
  signal(SIGILL, donosink);
  for (;;) {
    sleep(1);
    award_xp(0x5401640, 2000, 0, 1);
  }
}

__attribute__((constructor))
void ctor() {
  puts("constructor");
  std::thread t {award_thread};
  t.detach();
  puts("exit constructor");
}

__attribute__((destructor))
void dtor() {
  puts("destructor");
  puts("exit destructor");
}
