#include <stdio.h>
#include <stdint.h>
#include <fstream>
#include <sys/mman.h>
#include <string.h>

uintptr_t BASE_ADDR;
uintptr_t FUNC_ADDR;
uintptr_t DETOUR_ADDR;

uintptr_t get_base_addr() {
  std::ifstream maps("/proc/self/maps");
  std::string line;
  if (std::getline(maps, line)) {
    size_t pos = line.find('-');
    if (pos != std::string::npos) {
      std::string address_str = line.substr(0, pos);
      return std::stoul(address_str, nullptr, 16);
    }
  }
  perror("no base addr");
}

void print_bytes(uint8_t *ptr, size_t n) {
  while (0 < n) {
    for (int i = 0; i < 4 && 0 < n; i++, n--, ptr++) {
      printf("%x ", *ptr);
    }
    puts("");
  }
}

void patch(uintptr_t ptr, uint8_t *bytes, size_t n) {
  if(mprotect((void *)(ptr - ptr % 0x1000), 0x1000, PROT_READ | PROT_WRITE | PROT_EXEC) != 0) {
    perror(strerror(errno));
  }
  memcpy((void *)ptr, bytes, n);
  if(mprotect((void *)(ptr - ptr % 0x1000), 0x1000, PROT_READ | PROT_EXEC) != 0) {
    perror(strerror(errno));
  }
}

void detour() {
  puts("detour!");
  asm volatile (
    // "push %%ebp \n\t"
    // "mov %%esp,%%ebp \n\t"
    // "push %%ebx \n\t"
    // "sub $0x14,%%esp \n\t"
    "jmp *%0 \n\t" 
    : 
    : "r" (FUNC_ADDR) 
    :
  );
}

void set_addrs() {
  BASE_ADDR = get_base_addr();
  FUNC_ADDR = BASE_ADDR + 0x118d;
  DETOUR_ADDR = (uintptr_t)detour;
  printf("Base proc addr: [%p]\n", (void *)BASE_ADDR);
  printf("Func addr: [%p]\n", (void *)FUNC_ADDR);
  printf("Detour addr: [%p]\n", (void *)DETOUR_ADDR);
}

__attribute__((constructor))
void cons() {
  puts("constructor");
  set_addrs();
  uint8_t instr[5];
  instr[0] = 0xE9;
  *(uintptr_t *)(instr + 1) = DETOUR_ADDR - FUNC_ADDR - 1;
  patch(FUNC_ADDR, instr, 5);
  puts("exit constructor");
}