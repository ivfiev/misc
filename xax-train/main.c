#include <stdio.h>
#include <unistd.h>

int main(void) {
  char a[] = "ab";
  int local = 0x7921;
  char b[] = "bc";
  for (;;) {
    printf("%p %x\n", &local, local);
    sleep(1);
  }
  return 0;
}
