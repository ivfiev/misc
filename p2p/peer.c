#include "p2p.h"

extern int EPFD;

void exec_cmd(char *cmd, char **args, int argc) {
  if (!strcmp(cmd, "ping")) {
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
  if (read2(cb, buf) < 0) {
    return;
  }
  toks[0] = strtok(buf, delim);
  for (i = 1; toks[i - 1]; i++) {
    toks[i] = strtok(NULL, delim);
  }
  exec_cmd(toks[0], toks + 1, i);
}

void accept_peer(epoll_cb *cb) {
  int peer_fd = accept(cb->fd, NULL, NULL);
  printf("new connection [%d]\n", peer_fd);
  epoll_cb *peer_cb = alloc_cb(peer_fd);
  peer_cb->event.events = EPOLLIN | EPOLLHUP | EPOLLERR;
  peer_cb->on_EPOLLIN = peer_EPOLLIN;
  epoll_ctl(EPFD, EPOLL_CTL_ADD, peer_fd, &peer_cb->event);
}

void init_peer(char *port) {
  port = port ? port : "8080";
  int fd = listen1(port);
  printf("LISTENER port [%s], fd [%d]\n", port, fd);
  epoll_cb *cb = alloc_cb(fd);
  cb->event.events = EPOLLIN;
  cb->on_EPOLLIN = accept_peer;
  epoll_ctl(EPFD, EPOLL_CTL_ADD, fd, &cb->event);
}