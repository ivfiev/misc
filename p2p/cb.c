#include "p2p.h"

epoll_cb *alloc_cb(int fd) {
  epoll_cb *cb = (epoll_cb *)malloc(sizeof(epoll_cb));
  cb->fd = fd;
  memset(&cb->event, 0, sizeof(struct epoll_event));
  cb->event.data.ptr = cb;
  return cb;
}

void free_cb(epoll_cb *cb) {
  if (cb->buf_out) {
    free(cb->buf_out);
  }
  free(cb);
}