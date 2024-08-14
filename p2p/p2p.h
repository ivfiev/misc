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

    void (*on_EPOLLIN)(struct epoll_cb *cb);
} epoll_cb;

epoll_cb *alloc_cb(int fd);

void free_cb(epoll_cb *cb);

int listen1(const char *port);

int connect1(const char *port);

ssize_t read2(epoll_cb *cb, char *buf);

void close1(epoll_cb *cb);

void err(const char *msg);