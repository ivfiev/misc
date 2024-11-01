#include <iostream>

void func();

int calc(float x) {
  for (int i = 0; i < 10; i++) {
    x++;
  }
  return 5.0 * x;
}

int main() {
  printf("0x%p\n", &calc);
  func();
  std::cout << calc(10.0) << std::endl;
  return 0;
}
