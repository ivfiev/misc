#include <stdio.h>
#include <stdint.h>
#include <fstream>
#include <sys/mman.h>
#include <string.h>

uintptr_t BASE_ADDR;
uintptr_t FUNC_ADDR;
uintptr_t DETOUR_ADDR;

struct machine_code {
  uint8_t bytes[16];
  size_t size;
};

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

machine_code patch(uintptr_t ptr, machine_code code) {
  machine_code old;
  old.size = code.size;
  memcpy(old.bytes, (void *)ptr, old.size);
  if (mprotect((void *)(ptr - ptr % 0x1000), 0x1000, PROT_READ | PROT_WRITE | PROT_EXEC) != 0) {
    perror(strerror(errno));
  }
  memcpy((void *)ptr, code.bytes, code.size);
  if (mprotect((void *)(ptr - ptr % 0x1000), 0x1000, PROT_READ | PROT_EXEC) != 0) {
    perror(strerror(errno));
  }
  return old;
}

__attribute__((optimize("O0")))
void detour(void *self) {
  // do not declare any locals
  asm volatile (
    "push %%ebx \n\t"
    "sub $0x4,%%esp \n\t"
    :
    : 
    :
  );
  *(int *)self -= 2;
  asm volatile (
    "jmp *%0 \n\t" 
    : 
    : "r" (FUNC_ADDR + 7)
    :
  );
}

void set_addrs() {
  BASE_ADDR = get_base_addr();
  FUNC_ADDR = BASE_ADDR + 0x120c;
  DETOUR_ADDR = (uintptr_t)detour;
  printf("Base proc addr: [%p]\n", (void *)BASE_ADDR);
  printf("Func addr: [%p]\n", (void *)FUNC_ADDR);
  printf("Detour addr: [%p]\n", (void *)DETOUR_ADDR);
}

machine_code get_jmp_code() {
  machine_code code;
  code.size = 5;
  code.bytes[0] = 0xE9;
  *(uintptr_t *)(code.bytes + 1) = DETOUR_ADDR - FUNC_ADDR - code.size;
  return code;
}

class Hoock {
private:
  machine_code old;
public:
  Hoock() {
    set_addrs();
  }

  void write() {
    machine_code code = get_jmp_code();
    old = patch(FUNC_ADDR, code);
  }

  ~Hoock() {
    puts("restoring original code...");
    patch(FUNC_ADDR, old); 
    puts("code restored");
  }
};

Hoock *H = nullptr;

__attribute__((constructor))
void ctor() {
  puts("constructor");
  H = new Hoock;
  H->write();
  puts("exit constructor");
}

__attribute__((destructor))
void dtor() {
  puts("destructor");
  delete H;
  puts("exit destructor");
}
// static/dynamic ptr
// vtable hax