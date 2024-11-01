#include <stdio.h>

int func(int i, int j) {
  int k = i * j;
  puts("starting loop");
  while (k) {
    i += j;
    k--;
  }
  printf("(%d, %d) -> %d\n", i, j, j - i);
  return j - i;
}

int main() {
  printf("%d\n", func(10, 20));
  return 0;
}