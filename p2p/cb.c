#include <sys/timerfd.h>
#include "p2p.h"

extern int EPFD;
extern char *NAME;

epoll_cb *alloc_cb(int fd) {
  epoll_cb *cb = (epoll_cb *)malloc(sizeof(epoll_cb));
  memset(cb, 0, sizeof(epoll_cb));
  cb->fd = fd;
  cb->event.data.ptr = cb;
  return cb;
}

void free_cb(epoll_cb *cb) {
  if (cb->data != NULL) {
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
  printf("%s closing fd [%d]\n", NAME, cb->fd);
  if (cb->on_close != NULL) {
    cb->on_close(cb);
  }
  epoll_ctl(EPFD, EPOLL_CTL_DEL, cb->fd, NULL);
  if (close(cb->fd) < 0) {
    err_fatal("close1.close");
  }
  free_cb(cb);
}

static hashtable *timer_handlers = NULL;

int is_timer(int fd) {
  void *handler = hash_getv(timer_handlers, (void *)fd);
  return handler != NULL;
}

void timer_ack(epoll_cb *cb) {
  char buf[16];
  read(cb->fd, buf, sizeof(buf));
  void (*handler)(epoll_cb *cb) = hash_getv(timer_handlers, (void *)cb->fd);
  handler(cb);
}

int timer(long ms, void (*on_tick)(epoll_cb *cb), void *data) {
  int fd = timerfd_create(CLOCK_REALTIME, 0);
  if (timer_handlers == NULL) {
    timer_handlers = hash_new(16, hash_int, (int (*)(void *, void *))intcmp);
  }
  hash_set(timer_handlers, (void *)fd, on_tick);

  struct itimerspec *its = malloc(sizeof(struct itimerspec));
  epoll_cb *cb = alloc_cb(fd);
  its->it_value.tv_sec = ms / 1000;
  its->it_value.tv_nsec = 1000000 * (ms % 1000);
  its->it_interval.tv_sec = ms / 1000;
  its->it_interval.tv_nsec = 1000000 * (ms % 1000);
  timerfd_settime(fd, 0, its, NULL);

  timer_data *tdata = malloc(sizeof(timer_data));
  tdata->its = its; // no need to free?
  tdata->data = data;
  cb->data = tdata;
  cb->on_EPOLLIN = timer_ack;
  cb->event.events = EPOLLIN;
  epoll_ctl(EPFD, EPOLL_CTL_ADD, fd, &cb->event);
  return fd;
}
