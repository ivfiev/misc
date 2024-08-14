#include "p2p.h"

void exec_cmd(char *cmd, char **args, int argc) {
  if (!strcmp(cmd, "msg")) {
    int fd = connect1(args[0]);
    write(fd, args[1], strlen(args[1]));
    close(fd);
  }
}

void peer_EPOLLIN(epoll_cb *cb) {
  char *toks[128];
  const char *delim = ",";
  int i;
  char buf[BUF_SIZE];
  ssize_t bytes = read2(cb, buf);
  if (bytes < 0) {
    return;
  }
  toks[0] = strtok(buf, delim);
  for (i = 1; toks[i - 1]; i++) {
    toks[i] = strtok(NULL, delim);
  }
  exec_cmd(toks[0], toks + 1, i);
}