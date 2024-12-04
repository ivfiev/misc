#include "advent.h"

int main(int argc, char **argv) {
  char *input[1024];
  size_t height = read_lines(argv[1], input, SIZEARR(input));
  size_t length = strlen(*input);
  uint64_t xmases = 0;
  uint64_t Xmases = 0;
  int ds[8][2] = {{1,0},{0,1},{1,1},{-1,0},{0,-1},{-1,-1},{1,-1},{-1,1}};
  for (int i = 0; i < height; i++) {
    for (int j = 0; j < strlen(input[i]); j++) {
      for (int d = 0; d < SIZEARR(ds); d++) {
        int k, di = i, dj = j;
        for (k = 0; k < strlen("XMAS"); k++) {
          if (!IN_RANGE(0, di, height - 1) || 
              !IN_RANGE(0, dj, length - 1) || 
              input[di][dj] != "XMAS"[k]) {
            break;
          }
          di += ds[d][0];
          dj += ds[d][1];
        }
        if (k == strlen("XMAS")) {
          xmases++;
        }
      }
      if (IN_RANGE(1, i, height - 2) && 
          IN_RANGE(1, j, length - 2) &&
          (input[i-1][j-1] == 'M' && input[i+1][j+1] == 'S' || input[i-1][j-1] == 'S' && input[i+1][j+1] == 'M') && 
          input[i][j] == 'A' && 
          (input[i+1][j-1] == 'M' && input[i-1][j+1] == 'S' || input[i+1][j-1] == 'S' && input[i-1][j+1] == 'M')) {
        Xmases++;
      } 
    }
  }
  printf("Part 1: [%ld]\n", xmases);
  printf("Part 2: [%ld]\n", Xmases);
  free_lines(input, height);
  return 0;
}