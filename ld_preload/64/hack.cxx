#include <iostream>
#include <dlfcn.h>
#include <cstdint>
#include <fstream>
#include <sys/mman.h>
#include <string.h>

uintptr_t get_base_addr() {
  std::ifstream maps("/proc/self/maps");
  std::string line;
  uintptr_t base_address = 0;
  if (std::getline(maps, line)) {
    size_t pos = line.find('-');
    if (pos != std::string::npos) {
      std::string address_str = line.substr(0, pos);
      base_address = std::stoul(address_str, nullptr, 16);
    }
  }
  return base_address;
}

void func() {
  puts("Overriden func()");
  auto original = (void (*)())dlsym(RTLD_NEXT, "_Z4funcv");
  printf("entering original %p\n", original);
  original();
  puts("exited original");
  puts("exiting overriden func()");
}

void detour(float x) {
  for (;;) {}
}

__attribute__((constructor))
void cons() {
  puts("constructor");
  uintptr_t base_addr = get_base_addr();
  printf("Base addr: 0x%lx\n", base_addr);
  auto calc = (int (*)(float))(base_addr + 0x11a9);
  if(mprotect((void *)(base_addr + 4096), 4096, PROT_READ | PROT_WRITE | PROT_EXEC) != 0) {
    perror(strerror(errno));
  }
  printf("detour %p %p\n", (void *)(*(uintptr_t *)detour), (void *)(*(uintptr_t *)(detour + 8)));
  printf("calc %p %p\n", (void *)(*(uintptr_t *)calc), (void *)(*(uintptr_t *)(calc + 8)));
  memcpy((void *)calc, (void *)detour, 200);
  puts("after memcpy");
  mprotect((void *)(base_addr + 4096), 4096, PROT_READ | PROT_EXEC);
  puts("cons calling calc....");
  printf("%p\n", (void *)(*(uintptr_t *)calc));
  printf("cons %d\n", calc(1234));
  puts("exit constructor");
}
