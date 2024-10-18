


//static void compare(void) {
//  int fd = open("bytes.txt", O_RDONLY);
//  if (fd <= 0) {
//    err_fatal("read");
//  }
//  ssize_t size = read(fd, FILEBUF, SIZEARR(FILEBUF));
//  FILEBUF[size] = 0;
//  size_t lines = strsplit(FILEBUF, "\n", LINE_STRS, 10);
//  for (int i = 0; i < lines; i++) {
//    size_t line_len = strsplit(LINE_STRS[i], " ", BYTE_STRS[i], ADDR_LEN);
//    printf("Line %d with %zu bytes\n", i, line_len);
//  }
//  for (int i = 0; i < ADDR_LEN; i++) {
//    int j;
//    for (j = 0; j < lines - 1; j++) {
//      if (!strcmp(BYTE_STRS[j][i], "00") || strcmp(BYTE_STRS[j][i], BYTE_STRS[j + 1][i])) {
//        break;
//      }
//    }
//    if (j == lines - 1) {
//      printf("%d ", i);
//    }
//  }
//  printf("\n");
//}
//
//static void pattern(void) {
//  char *indexes_str = args_get("arg0");
//  char *indexes_strs[512];
//  int fd = open("bytes.txt", O_RDONLY);
//  if (fd <= 0) {
//    err_fatal("read");
//  }
//  ssize_t size = read(fd, FILEBUF, SIZEARR(FILEBUF));
//  FILEBUF[size] = 0;
//  size_t lines = strsplit(FILEBUF, "\n", LINE_STRS, 10);
//  strsplit(LINE_STRS[0], " ", BYTE_STRS[0], ADDR_LEN);
//  size_t ixs = strsplit(indexes_str, " ", indexes_strs, SIZEARR(indexes_strs));
//  for (int i = 0; i < ixs; i++) {
//    int ix = (int)strtol(indexes_strs[i], NULL, 10);
//    int byte = (uint8_t)strtol(BYTE_STRS[0][ix], NULL, 16);
//    printf("%d, ", byte);
//    if (i < ixs - 1) {
//      int ix2 = (int)strtol(indexes_strs[i + 1], NULL, 10);
//      for (int j = 0; j < ix2 - ix - 1; j++) {
//        printf("-1, ");
//      }
//    }
//  }
//  printf("\n");
//}