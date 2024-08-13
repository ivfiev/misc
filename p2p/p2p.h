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

    size_t buf_size;
    size_t buf_i;
    char *buf_out;
} epoll_cb;

epoll_cb *alloc_cb(int fd);

void free_cb(epoll_cb *cb);

int peer_listen(const char *port);

void err(const char *msg);

void reset_out_buf(epoll_cb *cb);