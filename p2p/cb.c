#include "p2p.h"

extern int EPFD;

epoll_cb *alloc_cb(int fd) {
  epoll_cb *cb = (epoll_cb *)malloc(sizeof(epoll_cb));
  memset(cb, 0, sizeof(epoll_cb));
  cb->fd = fd;
  cb->event.data.ptr = cb;
  return cb;
}

void free_cb(epoll_cb *cb) {
  if (cb->data) {
    free(cb->data);
  }
  free(cb);
}

ssize_t read2(epoll_cb *cb, char *buf) {
  ssize_t bytes = read(cb->fd, buf, BUF_SIZE);
  if (bytes <= 0) {
    close1(cb);
    return -1;
  }
  return bytes;
}

void close1(epoll_cb *cb) {
  printf("closing fd [%d]\n", cb->fd);
  if (cb->on_close) {
    cb->on_close(cb);
  }
  epoll_ctl(EPFD, EPOLL_CTL_DEL, cb->fd, NULL);
  close(cb->fd);
  free_cb(cb);
}