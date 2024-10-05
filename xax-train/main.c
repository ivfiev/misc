#include <stdio.h>
#include <unistd.h>

int main(void) {
  char a[] = "abcdefghijkl";
  int local = 0x7921;
  float local_2 = 123.45;
  char b[] = "xyz";
  for (;;) {
    printf("%p %x\n", &local, local);
    printf("%p %f\n", &local_2, local_2);
    sleep(1);
  }
  return 0;
}
