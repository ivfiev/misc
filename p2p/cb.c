#include "p2p.h"

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