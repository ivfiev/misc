#include "p2p.h"

epoll_cb *alloc_cb(int fd) {
  epoll_cb *cb = (epoll_cb *)malloc(sizeof(epoll_cb));
  cb->fd = fd;
  memset(&cb->event, 0, sizeof(struct epoll_event));
  cb->event.data.ptr = cb;
  reset_bufs(cb);
  return cb;
}

void free_cb(epoll_cb *cb) {
  if (cb->buf_out) {
    free(cb->buf_out);
  }
  if (cb->buf_in) {
    free(cb->buf_in);
  }
  free(cb);
}

void reset_bufs(epoll_cb *cb) {
  cb->buf_out = NULL;
  cb->buf_out_size = 0;
  cb->buf_out_i = 0;
  cb->buf_in = NULL;
  cb->buf_in_size = 0;
  cb->buf_in_i = 0;
}