#include "p2p.h"

extern int EPFD;

void peer_EPOLLIN(epoll_cb *cb);

void accept_peer(epoll_cb *cb) {
  int peer_fd = accept(cb->fd, NULL, NULL);
  printf("new connection [%d]\n", peer_fd);
  epoll_cb *peer_cb = alloc_cb(peer_fd);
  peer_cb->event.events = EPOLLIN | EPOLLHUP | EPOLLERR;
  peer_cb->on_EPOLLIN = peer_EPOLLIN;
  epoll_ctl(EPFD, EPOLL_CTL_ADD, peer_fd, &peer_cb->event);
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