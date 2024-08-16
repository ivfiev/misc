#include <sys/timerfd.h>
#include "p2p.h"

extern int EPFD;

int timer_interval(long ms, void (*on_tick)(epoll_cb *cb)) {
  int fd = timerfd_create(CLOCK_REALTIME, 0);
  struct itimerspec *its = malloc(sizeof(struct itimerspec));
  epoll_cb *cb = alloc_cb(fd);
  its->it_value.tv_sec = ms / 1000;
  its->it_value.tv_nsec = 1000000 * (ms % 1000);
  its->it_interval.tv_sec = ms / 1000;
  its->it_interval.tv_nsec = 1000000 * (ms % 1000);
  timerfd_settime(fd, 0, its, NULL);
  cb->data = its;
  cb->on_EPOLLIN = on_tick;
  cb->event.events = EPOLLIN;
  epoll_ctl(EPFD, EPOLL_CTL_ADD, fd, &cb->event);
  return fd;
}