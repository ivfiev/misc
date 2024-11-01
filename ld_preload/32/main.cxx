#include <stdio.h>

int func(int i, int j) {
  int k = i * j;
  while (k) {
    i += j;
    k--;
  }
  printf("%d\n", j - i);
  return j - i;
}

int main() {
  printf("%d\n", func(10, 20));
  return 0;
}