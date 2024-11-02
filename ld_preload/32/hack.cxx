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
  return 0;
}

void print_bytes(uint8_t *ptr, size_t n) {
  while (0 < n) {
    for (int i = 0; i < 4 && 0 < n; i++, n--, ptr++) {
      printf("%.2x ", *ptr);
    }
    puts("");
  }
}

void patch(uintptr_t ptr, uint8_t *bytes, size_t n) {
  if (mprotect((void *)(ptr - ptr % 0x1000), 0x1000, PROT_READ | PROT_WRITE | PROT_EXEC) != 0) {
    perror(strerror(errno));
  }
  memcpy((void *)ptr, bytes, n);
  if (mprotect((void *)(ptr - ptr % 0x1000), 0x1000, PROT_READ | PROT_EXEC) != 0) {
    perror(strerror(errno));
  }
}

void detour() {
  asm volatile (
    "push %%ebx \n\t"
    "sub $0x14,%%esp \n\t"
  : : :);
  puts("detour!");
  printf("0x%x\n", get_base_addr());
  printf("0x%x\n", BASE_ADDR);
  asm volatile (
    "jmp *%0 \n\t" 
    : 
    : "r" (FUNC_ADDR + 7)
    :
  );
}

void set_addrs() {
  BASE_ADDR = get_base_addr();
  FUNC_ADDR = BASE_ADDR + 0x119d;
  DETOUR_ADDR = (uintptr_t)detour;
  printf("Base proc addr: [%p]\n", (void *)BASE_ADDR);
  printf("Func addr: [%p]\n", (void *)FUNC_ADDR);
  printf("Detour addr: [%p]\n", (void *)DETOUR_ADDR);
}

__attribute__((constructor))
void ctor() {
  puts("constructor");
  // set_addrs();
  // uint8_t instr[5];
  // instr[0] = 0xE9;
  // *(uintptr_t *)(instr + 1) = DETOUR_ADDR - FUNC_ADDR - sizeof(instr);
  // patch(FUNC_ADDR, instr, sizeof(instr));
  // print_bytes((uint8_t *)DETOUR_ADDR, 20);
  puts("exit constructor");
}


__attribute__((destructor))
void dtor() {
  puts("destructor");
  puts("exit destructor");
}
// static/dynamic ptr
// access func args
// vtable hax