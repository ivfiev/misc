#include "p2p.h"

#define EPOLL_MAX_EVENTS 128
#define BUF_SIZE 4096

int main(int argc, char **argv) {
  int listenerfd = listener("8080");
  printf("listener fd %d\n", listenerfd);
  struct epoll_event events[EPOLL_MAX_EVENTS];
  char buf[BUF_SIZE];
  memset(events, 0, sizeof(events));
  memset(buf, 0, sizeof(buf));

  int epfd = epoll_create1(EPOLL_CLOEXEC);

  epoll_cb *listener_cb = alloc_cb(listenerfd);
  listener_cb->event.events = EPOLLIN;
  epoll_ctl(epfd, EPOLL_CTL_ADD, listenerfd, &listener_cb->event);

  for (;;) {
    int ready = epoll_wait(epfd, events, EPOLL_MAX_EVENTS, -1);

    for (int i = 0; i < ready; i++) {
      epoll_cb *cb = events[i].data.ptr;

      if (events[i].events & EPOLLIN) {
        if (cb->fd == listenerfd) {
          int clientfd = accept(listenerfd, NULL, NULL);
          printf("new connection %d\n", clientfd);
          epoll_cb *new_cb = alloc_cb(clientfd);
          new_cb->event.events = EPOLLIN | EPOLLHUP | EPOLLERR;
          epoll_ctl(epfd, EPOLL_CTL_ADD, clientfd, &new_cb->event);
        } else {
          ssize_t bytes = read(cb->fd, buf, sizeof(buf));
          if (bytes == 0) {
            goto CLOSE;
          }
          printf("incoming data from %d: %.*s", cb->fd, (int)bytes, buf);
          cb->buf_size = bytes + 20;
          cb->buf_out = calloc(sizeof(char), cb->buf_size);
          snprintf(cb->buf_out, cb->buf_size, "received - %.*s", (int)bytes, buf);
          cb->event.events |= EPOLLOUT;
          epoll_ctl(epfd, EPOLL_CTL_MOD, cb->fd, &cb->event);
        }
      }

      if (events[i].events & EPOLLOUT) {
        ssize_t sent = write(cb->fd, cb->buf_out, cb->buf_size);
        if (sent == cb->buf_size) {
          free(cb->buf_out);
          cb->buf_out = NULL;
          cb->buf_size = 0;
          cb->event.events ^= EPOLLIN;
          epoll_ctl(epfd, EPOLL_CTL_MOD, cb->fd, &cb->event);
        } else {
          err("write");
        }
      }

      if (events[i].events & (EPOLLHUP | EPOLLERR)) {
        CLOSE:
        printf("closing socket %d\n", cb->fd);
        epoll_ctl(epfd, EPOLL_CTL_DEL, cb->fd, NULL);
        close(cb->fd);
        free_cb(cb);
      }
    }
  }

  return 0;
}
