#include <stdio.h>
#include <sys/epoll.h>
#include <stdlib.h>
#include <sys/socket.h>
#include <netdb.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>

typedef struct epoll_cb {
    int fd;
    struct epoll_event event;

    char *buf_out;
    size_t buf_size;
} epoll_cb;

epoll_cb *alloc_cb(int fd);

void free_cb(epoll_cb *cb);

int listener(const char *port);

void err(const char *msg);