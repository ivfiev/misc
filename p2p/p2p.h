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

    size_t buf_out_size;
    size_t buf_out_i;
    char *buf_out;

    size_t buf_in_size;
    size_t buf_in_i;
    char *buf_in;
} epoll_cb;

epoll_cb *alloc_cb(int fd);

void free_cb(epoll_cb *cb);

int peer_listen(const char *port);

int peer_connect(const char *port);

void err(const char *msg);

void handle_msg(epoll_cb *cb);