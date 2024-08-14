#include "p2p.h"

void exec_command(char *cmd, char **args, int argc) {
  if (!strcmp(cmd, "msg")) {
    int fd = peer_connect(args[0]);
    write(fd, args[1], strlen(args[1]));
    close(fd);
  }
}

void handle_msg(epoll_cb *cb) {
  char *toks[128];
  const char *delim = ",";
  int i;
  toks[0] = strtok(cb->buf_in, delim);
  for (i = 1; toks[i - 1]; i++) {
    toks[i] = strtok(NULL, delim);
  }
  exec_command(toks[0], toks + 1, i);
}