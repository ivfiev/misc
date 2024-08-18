#include "p2p.h"

extern int EPFD;

char NAME[8];
static hashtable *peers;

void init_peer(int fd);

void exec_cmd(char *cmd, char **args, int argc) {
  if (!strcmp(cmd, "ping")) {
    printf("pinging [%s] [%s]\n", args[0], args[1]);
    int fd = (int)hash_get(peers, args[0]);
    if (fd) {
      write(fd, args[1], strlen(args[1]));
    }
  } else if (!strcmp(cmd, "peers")) {
    for (int i = 0; i < argc; i++) {
      for (int j = 0; args[i][j]; j++)
        if (args[i][j] == '\n')
          args[i][j] = 0;
    }
    for (int i = 0; i < argc; i++) {
      int fd = connect1(args[i]);
      if (fd > 0) {
        init_peer(fd);
      }
    }
  }
}

void peer_EPOLLIN(epoll_cb *cb) {
  char *toks[128];
  const char *delim = ",";
  int i;
  char buf[BUF_SIZE];
  ssize_t bytes = read2(cb, buf);
  if (bytes < 0) {
    // cb disconnected
    return;
  }
  buf[bytes] = 0;
  toks[0] = strtok(buf, delim);
  for (i = 1; toks[i - 1]; i++) {
    toks[i] = strtok(NULL, delim);
  }
  exec_cmd(toks[0], toks + 1, i == 1 ? 0 : i - 2);
}

void disconnect_peer(epoll_cb *cb) {
  free(hash_del(peers, cb->data));
}

void peer_tick(epoll_cb *cb) {
  printf("tick\n");
}

void accept_peer(epoll_cb *cb) {
  int peer_fd = accept(cb->fd, NULL, NULL);
  init_peer(peer_fd);
}

void init_peer(int peer_fd) {
  char *peer_name = getname(peer_fd);
  hash_set(peers, peer_name, (void *)peer_fd);
  printf("new connection fd [%d] name [%s]\n", peer_fd, peer_name);

  epoll_cb *peer_cb = alloc_cb(peer_fd);
  peer_cb->event.events = EPOLLIN | EPOLLHUP | EPOLLERR;
  peer_cb->data = peer_name;
  peer_cb->on_EPOLLIN = peer_EPOLLIN;
  peer_cb->on_close = disconnect_peer;
  epoll_ctl(EPFD, EPOLL_CTL_ADD, peer_fd, &peer_cb->event);
}

void init(char *port) {
  port = port ? port : "8080";
  int fd = listen1(port);
  printf("LISTENER port [%s], fd [%d]\n", port, fd);
  epoll_cb *cb = alloc_cb(fd);
  cb->event.events = EPOLLIN;
  cb->on_EPOLLIN = accept_peer;
  epoll_ctl(EPFD, EPOLL_CTL_ADD, fd, &cb->event);

  strcpy(NAME, port);
  peers = hash_new(128, hash_str, (int (*)(void *, void *))strcmp);

  timer(5000, peer_tick);
}