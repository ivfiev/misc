#include <stdio.h>
#include <unistd.h>
#include <malloc.h>
#include <stdlib.h>

struct player {
  float x;
};

int main(void) {
  char a[] = "abcdefghijkl";
  int local = 0x7921;
  float local_2 = 123.45;
  char b[] = "xyz";

  struct player *p = malloc(sizeof(struct player));
  p->x = 0;
  float mul = 1;

  for (;;) {
    printf("%p %x\n", &local, local);
    printf("%p %f\n", &local_2, local_2);
    printf("%p %f\n", &p->x, p->x);
    printf("\n");
    mul = p->x > 1 ? -1 : p->x < -1 ? 1 : mul;
    p->x += 0.1f * mul;
    sleep(1);
  }
  return 0;
}
