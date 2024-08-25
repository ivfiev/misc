#include <time.h>
#include "p2p.h"

int EPFD;

void init(char *port);

void init_log(char *port);

int is_timer(int fd);

int cmp_events(struct epoll_event *e1, struct epoll_event *e2) {
  epoll_cb *cb1 = e1->data.ptr;
  epoll_cb *cb2 = e2->data.ptr;
  printf("qsort %d %d\n", cb1->fd, cb2->fd);
  int t1 = is_timer(cb1->fd);
  int t2 = is_timer(cb2->fd);
  return t1 - t2;
}

int main(int argc, char **argv) {
  srand(time(NULL));
  struct epoll_event events[EPOLL_MAX_EVENTS];
  memset(events, 0, sizeof(events));

  EPFD = epoll_create1(EPOLL_CLOEXEC);

  if (argc > 2 && !strcmp(argv[2], "--logs") && argv[3]) {
    init_log(argv[3]);
  }

  init(argv[1]);

  for (;;) {
    int ready = epoll_wait(EPFD, events, EPOLL_MAX_EVENTS, -1);

    qsort(events, ready, sizeof(struct epoll_event), (__compar_fn_t)cmp_events);

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

// negative numbers in peer fds,