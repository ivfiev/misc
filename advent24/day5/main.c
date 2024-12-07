#include "advent.h"
#define MAX_PAGES 100

static int BEFORE[MAX_PAGES][MAX_PAGES];

size_t parse_order(char **lines, size_t count) {
  for (size_t i = 0; i < count; i++) {
    if (!strcmp("\n", lines[i])) {
      return i;
    }
    int a, b;
    sscanf(lines[i], "%d|%d", &a, &b);
    BEFORE[a][b] = 1;
  }
}

int cmp_pages(const void *i, const void *j) {
  if (BEFORE[*(int *)i][*(int *)j]) {
    return -1;
  } else if (BEFORE[*(int *)j][*(int *)i]) {
    return 1;
  } else {
    return 0;
  }
}

int is_valid(int *pages, size_t size) {
  for (int i = 0; i < size; i++) {
    for (int j = i + 1; j < size; j++) {
      if (cmp_pages(&pages[j], &pages[i]) < 0) {
        return 0;
      }
    }
  }
  return 1;
}

int main(int argc, char **argv) {
  char *input[4096];
  size_t lines = read_lines(argv[1], input, SIZEARR(input));
  size_t rules = parse_order(input, lines);
  uint64_t sum1 = 0, sum2 = 0;
  for (int i = rules + 1; i < lines; i++) {
    int pages[MAX_PAGES];
    PARSE_LINE(input[i], ",", MAX_PAGES, { pages[i] = atoi(token); });
    if (is_valid(pages, count)) {
      sum1 += pages[count / 2];
    } else {
      qsort(pages, count, sizeof(int), cmp_pages);
      sum2 += pages[count / 2];
    }
  }
  printf("Part 1: [%ld]\n", sum1);
  printf("Part 2: [%ld]\n", sum2);
  free_lines(input, lines);
  return 0;
}