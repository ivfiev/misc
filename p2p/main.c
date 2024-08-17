#include "p2p.h"

int EPFD;

void init_peer(char *port);

void init_log(char *port);

int main(int argc, char **argv) {
  struct epoll_event events[EPOLL_MAX_EVENTS];
  memset(events, 0, sizeof(events));

  EPFD = epoll_create1(EPOLL_CLOEXEC);

  if (!strcmp(argv[2], "--logs") && argv[3]) {
    init_log(argv[3]);
  }

  init_peer(argv[1]);

  for (;;) {
    int ready = epoll_wait(EPFD, events, EPOLL_MAX_EVENTS, -1);

    for (int i = 0; i < ready; i++) {
      epoll_cb *cb = events[i].data.ptr;

      if (events[i].events & EPOLLIN) {
        cb->on_EPOLLIN(cb);
      }

      if (events[i].events & (EPOLLHUP | EPOLLERR)) {
        close1(cb);
      }
    }
  }
}