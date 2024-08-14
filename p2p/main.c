#include "p2p.h"

int EPFD;

int main(int argc, char **argv) {
  int listener_fd = peer_listen(argv[1] ? argv[1] : "8080");
  printf("listener fd %d\n", listener_fd);
  struct epoll_event events[EPOLL_MAX_EVENTS];
  char buf[BUF_SIZE];
  memset(events, 0, sizeof(events));
  memset(buf, 0, sizeof(buf));

  EPFD = epoll_create1(EPOLL_CLOEXEC);

  epoll_cb *listener_cb = alloc_cb(listener_fd);
  listener_cb->event.events = EPOLLIN;
  epoll_ctl(EPFD, EPOLL_CTL_ADD, listener_fd, &listener_cb->event);

  for (int i = 2; i < argc; i++) {
    int peer_fd = peer_connect(argv[i]);
    epoll_cb *peer_cb = alloc_cb(peer_fd);
    peer_cb->event.events = EPOLLIN;
    epoll_ctl(EPFD, EPOLL_CTL_ADD, peer_fd, &peer_cb->event);
  }

  for (;;) {
    int ready = epoll_wait(EPFD, events, EPOLL_MAX_EVENTS, -1);

    for (int i = 0; i < ready; i++) {
      epoll_cb *cb = events[i].data.ptr;

      if (events[i].events & EPOLLIN) {
        if (cb->fd == listener_fd) {
          int client_fd = accept(listener_fd, NULL, NULL);
          printf("new connection %d\n", client_fd);
          epoll_cb *new_cb = alloc_cb(client_fd);
          new_cb->event.events = EPOLLIN | EPOLLHUP | EPOLLERR;
          epoll_ctl(EPFD, EPOLL_CTL_ADD, client_fd, &new_cb->event);
        } else {
          ssize_t bytes = read(cb->fd, buf, sizeof(buf));
          if (bytes == 0) {
            goto CLOSE;
          }
          printf("incoming data from %d: %.*s", cb->fd, (int)bytes, buf);
          cb->buf_in_size = bytes;
          cb->buf_in = calloc(sizeof(char), bytes + 1);
          memcpy(cb->buf_in, buf, bytes);
          handle_msg(cb);
//          cb->buf_out_size = bytes + 20;
//          cb->buf_out = calloc(sizeof(char), cb->buf_out_size);
//          snprintf(cb->buf_out, cb->buf_out_size, "received - %.*s", (int)bytes, buf);
//          cb->event.events |= EPOLLOUT;
//          epoll_ctl(EPFD, EPOLL_CTL_MOD, cb->fd, &cb->event);
        }
      }

      if (events[i].events & EPOLLOUT) {
        ssize_t sent = write(cb->fd, cb->buf_out + cb->buf_out_i, cb->buf_out_size - cb->buf_out_i);
        if (sent == cb->buf_out_size) {
//          free(cb->buf_out);
//          reset_bufs(cb);
//          cb->event.events ^= EPOLLOUT;
//          epoll_ctl(EPFD, EPOLL_CTL_MOD, cb->fd, &cb->event);
        } else {
//          cb->buf_out_i += sent;
        }
      }

      if (events[i].events & (EPOLLHUP | EPOLLERR)) {
        CLOSE:
        printf("closing socket %d\n", cb->fd);
        epoll_ctl(EPFD, EPOLL_CTL_DEL, cb->fd, NULL);
        close(cb->fd);
        free_cb(cb);
      }
    }
  }

  return 0;
}
