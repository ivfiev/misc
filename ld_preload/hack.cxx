#include <iostream>
#include <dlfcn.h>

void func() {
  std::cout << "func override\n";
  auto original = (void (*)())dlsym(RTLD_NEXT, "_Z4funcv");
  original();
}

__attribute__((constructor))
void cons() {
  std::cout << "constructor\n";
}
