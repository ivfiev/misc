#include "p2p.h"

extern int EPFD;

void peer_EPOLLIN(epoll_cb *cb);

void accept_peer(epoll_cb *cb) {
  int client_fd = accept(cb->fd, NULL, NULL);
  printf("new connection %d\n", client_fd);
  epoll_cb *new_cb = alloc_cb(client_fd);
  new_cb->event.events = EPOLLIN | EPOLLHUP | EPOLLERR;
  new_cb->on_EPOLLIN = peer_EPOLLIN;
  epoll_ctl(EPFD, EPOLL_CTL_ADD, client_fd, &new_cb->event);
}

void init_listener(char *port) {
  port = port ? port : "8080";
  int fd = listen1(port);
  printf("LISTENER port [%s], fd [%d]\n", port, fd);
  epoll_cb *cb = alloc_cb(fd);
  cb->event.events = EPOLLIN;
  cb->on_EPOLLIN = accept_peer;
  epoll_ctl(EPFD, EPOLL_CTL_ADD, fd, &cb->event);
}