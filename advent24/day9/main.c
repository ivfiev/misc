#include "advent.h"

int main(int argc, char **argv) {
#define num(i) (input[0][i] - '0')
  char *input[1];
  read_lines(argv[1], input, 1);
  size_t length = strlen(*input);
  int *fs1 = calloc(length * 10, sizeof(int));
  int *fs2 = calloc(length * 10, sizeof(int));
  uint64_t part1 = 0, part2 = 0, fs_len = 0;
  for (int i = 0; i < length; i++) {
    int count = num(i);
    for (int j = 0; j < count; j++) {
      int id = i & 1 ? -1 : i / 2;
      fs1[fs_len] = id;
      fs2[fs_len++] = id;
    }
  }
  int i = 0, j = fs_len - 1, len_id, len_1;
  while (i < j) {
    while (i < j && fs1[i] == -1 && fs1[j] != -1) {
      fs1[i++] = fs1[j];
      fs1[j--] = -1;
    }
    while (i < j && fs1[i] != -1) { i++; }
    while (i < j && fs1[j] == -1) { j--; }
  }
  for (int curr_id = (length - 1) / 2; 0 < curr_id; curr_id--) {
    for (i = 0; fs2[i] != curr_id; i++);
    for (len_id = 0; fs2[i + len_id] == curr_id; len_id++);
    for (j = 0; j < i; j++) {
      for (; j < i && fs2[j] != -1; j++);
      for (len_1 = 0; fs2[j + len_1] == -1; len_1++);
      if (len_1 >= len_id) {
        for (int i = 0; i < len_id; i++)
          fs2[j + i] = curr_id;
        while (fs2[i] == curr_id)
          fs2[i++] = -1;
        break;
      }
    }
  }
  for (int i = 0; i < length * 10; i++) {
    part1 += MAX(fs1[i], 0) * i;
    part2 += MAX(fs2[i], 0) * i;
  }
  printf("Part 1: [%ld]\n", part1);
  printf("Part 2: [%ld]\n", part2);
  free_lines(input, 1);
  free(fs1);
  free(fs2);
  return 0;
}