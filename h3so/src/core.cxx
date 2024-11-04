#include "h3so.hxx"
#include "stdint.h"
#include <unistd.h>
#include <thread>
#include <stdexcept>

#define STATIC_PTR 0x699538

typedef void (*award_xp_t)(uint32_t, uint32_t, uint32_t);
static award_xp_t AWARD_XP_FUNC = (award_xp_t)0x4e3620;

class H {
private:
  uintptr_t addr;
public:
  H(uintptr_t a) {
    addr = a;
  }
  ~H() {}

  __attribute__((optimize("O0")))
  void award_xp(uint32_t xp) {
    asm("mov %0, %%ecx" : : "r"(addr) : "%ecx");
    AWARD_XP_FUNC(xp, 0, 0);
  }
};

static H *Hs[156];

void award_thread() {
  for (;;) {
    sleep(5);
    for (auto h : Hs) {
      h->award_xp(1000);
    }
  }
}

void init() {
  uintptr_t state = *(uintptr_t *)STATIC_PTR + 0x21620;
  log("state: [0x%lx]", state);
  for (int i = 0; i < 156; i++) {
    Hs[i] = new H(state + i * 0x492);
  }
}

void inject() {
  init();
  std::thread {award_thread}.detach();
}

void unject() {
}