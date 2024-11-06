#include "h3so.hxx"
#include "stdint.h"
#include "math.h"
#include <unistd.h>
#include <thread>
#include <stdexcept>

#define STATIC_PTR 0x699538
#define H_COUNT 156

typedef void (*award_xp_t)(uint32_t, uint32_t, uint32_t);
static award_xp_t AWARD_XP_FUNC = (award_xp_t)0x4e3620;
static uintptr_t NEW_DAY_FUNC_PTR = (uintptr_t)0x4c7ca0;

class H {
private:
  uintptr_t addr;
public:
  H(uintptr_t a) {
    addr = a;
  }
  ~H() {}

  uint get_xp() {
    return *(uint *)(addr + 0x51);
  }

  __attribute__((optimize("O0")))
  void award_xp(uint32_t xp) {
    asm("mov %0, %%ecx" : : "r"(addr) : "%ecx");
    AWARD_XP_FUNC(xp, 0, 0);
  }
};

static H *Hs[H_COUNT];

void init_hs() {
  uintptr_t state = *(uintptr_t *)STATIC_PTR + 0x21620;
  log("state: [0x%lx]", state);
  for (int i = 0; i < H_COUNT; i++) {
    Hs[i] = new H(state + i * 0x492);
  }
}

void new_day_award_xp() {
  for (auto h : Hs) {
    uint award = (uint)floor(0.01 * (double)h->get_xp());
    award = award > 1 ? award : 1;
    h->award_xp(award);
  }
}

__attribute__((optimize("O0")))
void new_day_detour() {
  // __fastcall ecx & edx
  asm volatile(
    "push %%edx \n\t"
    "push %%ecx \n\t"
    : : :);
  new_day_award_xp();
  asm volatile (
    "pop %%ecx \n\t"
    "pop %%edx \n\t"
    "add $0x4,%%esp \n\t"
    "pop %%ebx \n\t"
    "sub $0xc,%%esp \n\t"
    "push %%ebx \n\t"
    "jmp *%0 \n\t" 
    : 
    : "r" (NEW_DAY_FUNC_PTR + 7)
    :
  );
}

void init_detour() {
  binary code = get_jmp_code(NEW_DAY_FUNC_PTR, (uintptr_t)new_day_detour, 7);
  patch_rx(NEW_DAY_FUNC_PTR, code);
}

void inject() {
  init_hs();
  init_detour();
}

void unject() {
}