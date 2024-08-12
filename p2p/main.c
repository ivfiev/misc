#include <stdio.h>
#include <sys/epoll.h>
#include <stdlib.h>
#include <sys/socket.h>
#include <netdb.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>

#define EPOLL_MAX_EVENTS 128
#define BUF_SIZE 4096

typedef struct epoll_cb {
    int fd;
    struct epoll_event event;
} epoll_cb;

void err(const char *msg) {
  printf("%s\n", msg);
  printf("%s\n", strerror(errno));
  exit(1);
}

epoll_cb *alloc_cb(int fd) {
  epoll_cb *cb = (epoll_cb *)malloc(sizeof(epoll_cb));
  cb->fd = fd;
  memset(&cb->event, 0, sizeof(struct epoll_event));
  cb->event.data.ptr = cb;
  return cb;
}

int listener(const char *port) {
  struct addrinfo hints;
  memset(&hints, 0, sizeof(hints));
  hints.ai_family = AF_INET;
  hints.ai_socktype = SOCK_STREAM;
  hints.ai_flags = AI_PASSIVE;
  struct addrinfo *bindaddr;
  if (getaddrinfo(0, "8080", &hints, &bindaddr) < 0) {
    err("getaddrinfo");
  }
  int listener = socket(AF_INET, SOCK_STREAM, 0);
  if (listener < 0) {
    err("listener");
  }
  if (bind(listener, bindaddr->ai_addr, bindaddr->ai_addrlen) < 0) {
    err("bind");
  }
  if (listen(listener, 0) < 0) {
    err("listen");
  }
  freeaddrinfo(bindaddr);
  return listener;
}

int main(void) {
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
          int bytes = (int)read(cb->fd, buf, sizeof(buf));
          if (bytes == 0) {
            goto CLOSE;
          }
          printf("incoming data from %d:\n", cb->fd);
          printf("%.*s", bytes, buf);
        }
      }
      if (events[i].events & (EPOLLHUP | EPOLLERR)) {
        CLOSE:
        printf("closing socket %d\n", cb->fd);
        epoll_ctl(epfd, EPOLL_CTL_DEL, cb->fd, NULL);
        close(cb->fd);
        free(cb);
      }
    }
  }

  return 0;
}
