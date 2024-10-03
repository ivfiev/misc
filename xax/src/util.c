#include <stdio.h>
#include <stdlib.h>

void err_fatal(char *s) {
  printf("%s\n", s);
  exit(1);
}