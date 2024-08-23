#include "p2p.h"

extern int EPFD;

char *NAME;
static hashtable *peers;
const int tick_ms = 5000;

int init_peer(int fd);

void exec_cmd(char *cmd, char **args, int argc, epoll_cb *cb) {
  for (int i = 0; i < argc; i++) {
    trim_end(args[i]);
  }
  if (!strcmp(cmd, "ping")) {
    printf("pinging [%s] [%s]\n", args[0], args[1]);
    int fd = (int)hash_getv(peers, args[0]);
    if (fd) {
      write(fd, args[1], strlen(args[1]));
    }
  } else if (!strcmp(cmd, "status")) {
    if (!hash_get(peers, args[0])) {
      char *peer_name = strdup(args[0]);
      hash_set(peers, peer_name, (void *)cb->fd);
      cb->data = peer_name;
    }
  } else if (!strcmp(cmd, "peers")) {
    for (int i = 0; i < argc; i++) {
      if (!strcmp(NAME, args[i])) {
        continue;
      }
      int fd = (int)hash_get(peers, args[i]);
      if (fd == 0) {
        fd = connect1(args[i]);
        if (!init_peer(fd)) {
          char *peer_name = strdup(args[i]);
          hash_set(peers, peer_name, (void *)fd);
        }
      }
    }
  }
}

void peer_EPOLLIN(epoll_cb *cb) {
  char *cmd, *cmd_r = NULL;
  char *toks[128];
  const char *delim_tok = ",";
  const char *delim_cmd = "\0";
  int i;
  char buf[BUF_SIZE];
  ssize_t bytes = read2(cb, buf);
  if (bytes < 0) {
    // cb disconnected
    return;
  }
  buf[bytes] = 0;
  cmd = strtok_r(buf, delim_cmd, &cmd_r);
  while (cmd != NULL) {
    toks[0] = strtok(buf, delim_tok);
    for (i = 1; toks[i - 1]; i++) {
      toks[i] = strtok(NULL, delim_tok);
    }
    exec_cmd(toks[0], toks + 1, i == 1 ? 0 : i - 2, cb);
    cmd = strtok_r(NULL, delim_cmd, &cmd_r);
  }
}

void disconnect_peer(epoll_cb *cb) {
  if (cb->data != NULL) {
    free(hash_del(peers, cb->data));
  }
}

void log_stats(char **keys, size_t len) {
  int total = 0, conn = 0;
  char conn_buf[BUF_SIZE], total_buf[BUF_SIZE];
  char *conn_ptr = conn_buf, *total_ptr = total_buf;
  for (int i = 0; i < len; i++) {
    int fd = (int)hash_getv(peers, keys[i]);
    if (fd > 0) {
      conn++;
      conn_ptr += snprintf(conn_ptr, 10, ",%s", keys[i]);
    }
    total++;
    total_ptr += snprintf(total_ptr, 10, ",%s", keys[i]);
  }
  if (total > 0) {
    printf("%s - %d / %d\nconn  - %s\ntotal - %s\n\n", NAME, conn, total, conn_buf + 1, total_buf + 1);
  } else {
    printf("%s - %d / %d\nconn  - \ntotal - \n\n", NAME, 0, 0);
  }
}

void peer_tick(epoll_cb *cb) {
  if (peers->len == 0) {
    return;
  }
  char **keys = (char **)hash_keys(peers);
  size_t per_min = peers->len * 3;
  size_t per_tick = (double)per_min / (60000.0 / tick_ms) + 1; // TODO divide by connection count
  char **gossip = (char **)rand_select((void **)keys, peers->len, per_tick);
  char peers_buf[BUF_SIZE];
  char status_buf[BUF_SIZE];
  char *ptr = peers_buf + snprintf(peers_buf, 20, "%s", "peers");
  for (int i = 0; i < per_tick; i++) {
    ptr += snprintf(ptr, 10, ",%s", gossip[i]);
  }
  snprintf(status_buf, sizeof(status_buf), "status,%s", NAME);
  for (int i = 0; i < peers->len; i++) {
    int fd = (int)hash_getv(peers, keys[i]);
    if (fd > 0) {
      write(fd, status_buf, strlen(status_buf) + 1);
      write(fd, peers_buf, strlen(peers_buf) + 1);
    }
  }
  log_stats(keys, peers->len);
  free(gossip);
  free(keys);
}

void accept_peer(epoll_cb *cb) {
  int peer_fd = accept(cb->fd, NULL, NULL);
  init_peer(peer_fd);
}

int init_peer(int peer_fd) {
  if (peer_fd <= 0) {
    err_info("init_peer");
    return -1;
  }
  epoll_cb *peer_cb = alloc_cb(peer_fd);
  peer_cb->event.events = EPOLLIN | EPOLLHUP | EPOLLERR;
  peer_cb->on_EPOLLIN = peer_EPOLLIN;
  peer_cb->on_close = disconnect_peer;
  epoll_ctl(EPFD, EPOLL_CTL_ADD, peer_fd, &peer_cb->event);
  return 0;
}

void init(char *port) {
  port = port ? port : "8080";
  int fd = listen1(port);
  printf("LISTENER port [%s], fd [%d]\n", port, fd);
  epoll_cb *cb = alloc_cb(fd);
  cb->event.events = EPOLLIN;
  cb->on_EPOLLIN = accept_peer;
  epoll_ctl(EPFD, EPOLL_CTL_ADD, fd, &cb->event);

  NAME = strdup(port);
  peers = hash_new(128, hash_str, (int (*)(void *, void *))strcmp);

  timer(tick_ms, peer_tick, NULL);
}