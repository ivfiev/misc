#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int get_cpus() {
  char buf[1024];
  char *line;
  FILE *fp;

  fp = popen("lscpu", "r"); 
  if (fp == NULL) {
    printf("failed to run lscpu");
    exit(1);
  }

  for (;;) {
    char *s = fgets(buf, sizeof(buf), fp);
    if (s == NULL) {
      printf("failed to read lscpu output");
      exit(1);
    }

    line = strcasestr(s, "CPU(s)");
    if (line != NULL) {
      break;
    }
  }

  while (*line && (*line < '0' || '9' < *line)) {
    line++;
  }

  if (!*line) {
    printf("failed to parse the number of CPUs");
    exit(1);
  }

  pclose(fp);

  return atoi(line);
}


int main(void) {
  int cpus = get_cpus();
  return 0;
}
