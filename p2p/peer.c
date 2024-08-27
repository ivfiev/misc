#include "p2p.h"

typedef struct peer_descriptor {
  int fd;
  epoll_cb *cb;
} PD;

extern int EPFD;

char *NAME;
static hashtable *peers;
const int tick_ms = 2000;

epoll_cb *init_peer(int fd);

PD *get_pd(int fd, epoll_cb *cb);

void clear_pd(PD *pd);

void exec_cmd(char *cmd, char **args, int argc, epoll_cb *cb) {
  for (int i = 0; i < argc; i++) {
    trim_end(args[i]);
  }
  if (!strcmp(cmd, "debug")) {
    if (!strcmp(args[0], "conn")) {
      log_debug("connecting...\n");
      int fd = connect1(args[1]);
      epoll_cb *peer_cb = init_peer(fd);
      if (peer_cb != NULL) {
        char *peer_name = strdup(args[1]); // just a debug cmd, so it's fine to alloc.
        peer_cb->data = peer_name;
        PD *pd = get_pd(fd, peer_cb);
        hash_set(peers, peer_name, pd);
      }
    } else if (!strcmp(args[0], "close")) {
      PD *pd = hash_getv(peers, args[1]);
      log_debug("disconnecting fd [%d] ...\n", pd->fd);
      close1(pd->cb);
      clear_pd(pd);
    }
  } else if (!strcmp(cmd, "peers")) {
    if (strcmp(args[0], "nc") != 0 && strcmp(args[0], NAME) != 0) {
      char *peer_name = hash_getk(peers, args[0]);
      if (peer_name == NULL) {
        peer_name = strdup(args[0]);
      }
      if (cb->data == NULL) {
        cb->data = peer_name;
      }
      PD *pd = hash_getv(peers, peer_name);
      if (pd == NULL) {
        pd = get_pd(cb->fd, cb);
      } else if (pd->fd <= 0) {
        pd->fd = cb->fd;
        pd->cb = cb;
      } else if (pd->fd != cb->fd) {
        if (strcmp(NAME, peer_name) < 0) {
          log_debug("double connection to %s, closing my end...\n", peer_name);
          close1(pd->cb);
          clear_pd(pd);
          pd->fd = cb->fd;
          pd->cb = cb;
        }
      }
      hash_set(peers, peer_name, pd);
    }
    for (int i = 1; i < argc; i++) {
      if (!strcmp(NAME, args[i]) || !strcmp(args[0], args[i])) {
        continue;
      }
      char *existing = (char *)hash_getk(peers, args[i]);
      if (existing == NULL) {
        existing = strdup(args[i]);
        hash_set(peers, existing, NULL);
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
  log_debug("disconnect_peer [%s]\n", (char *)cb->data);
  if (cb->data != NULL) {
    PD *pd = hash_getv(peers, cb->data);
    cb->data = NULL;
    if (pd->fd == cb->fd) {
      clear_pd(pd); // leave it in
    }
  }
}

void log_stats(char **keys, size_t len) {
  int total = 0, conn = 0;
  char conn_buf[BUF_SIZE], total_buf[BUF_SIZE];
  char *conn_ptr = conn_buf, *total_ptr = total_buf;
  for (int i = 0; i < len; i++) {
    PD *pd = hash_getv(peers, keys[i]);
    if (pd != NULL && pd->fd > 0) {
      conn++;
      conn_ptr += snprintf(conn_ptr, 10, ",%s", keys[i]);
    }
    total++;
    total_ptr += snprintf(total_ptr, 10, ",%s", keys[i]);
  }
  if (total > 0) {
    log_info("%d / %d\nconn  - %s\ntotal - %s\n\n", conn, total, conn_buf + 1, total_buf + 1);
  } else {
    log_info("%d / %d\nconn  - \ntotal - \n\n", 0, 0);
  }
}

void peer_reconnect(epoll_cb *cb) {
  if (peers->len <= 0) {
    return;
  }
  char **keys = (char **)hash_keys(peers);
  char *cands[peers->len];
  int c = 0;
  for (int i = 0; i < peers->len; i++) {
    PD *pd = hash_getv(peers, keys[i]);
    if (pd == NULL || pd->fd <= 0) {  // maybe include connected?
      cands[c++] = keys[i];
    } else {
      close1(pd->cb);
      clear_pd(pd);
    }
  }
  size_t count = (size_t)(log((double)peers->len) / 2.0 + 1);
  count = MIN(count, c);
  char **new = (char **)rand_select((void **)cands, c, count);
  for (int i = 0; i < count; i++) {
    log_debug("connecting to %s\n", new[i]);
    int fd = connect1(new[i]);
    epoll_cb *cb = init_peer(fd);
    if (cb != NULL) {
      PD *pd = hash_getv(peers, new[i]);
      if (pd == NULL) {
        pd = get_pd(fd, cb);
        hash_set(peers, new[i], pd);
      }
      pd->fd = fd;
      pd->cb = cb;
      cb->data = new[i];
    }
  }
  free(new);
  free(keys);
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
  char *ptr = peers_buf + snprintf(peers_buf, 20, "%s,%s", "peers", NAME);
  for (int i = 0; i < per_tick; i++) {
    ptr += snprintf(ptr, 10, ",%s", gossip[i]);
  }
  for (int i = 0; i < peers->len; i++) {
    PD *pd = hash_getv(peers, keys[i]);
    if (pd != NULL && pd->fd > 0) {
      write(pd->fd, peers_buf, strlen(peers_buf) + 1);
    }
  }
  log_stats(keys, peers->len);
  free(gossip);
  free(keys);
}

void accept_peer(epoll_cb *cb) {
  int peer_fd = accept(cb->fd, NULL, NULL);
  log_debug("accepting peer [%d]\n", peer_fd);
  init_peer(peer_fd); // we'll store the cbs in 'peers' handler
}

epoll_cb *init_peer(int peer_fd) {
  if (peer_fd <= 0) {
    err_info("init_peer");
    return NULL;
  }
  epoll_cb *peer_cb = alloc_cb(peer_fd);
  peer_cb->event.events = EPOLLIN | EPOLLHUP | EPOLLERR;
  peer_cb->on_EPOLLIN = peer_EPOLLIN;
  peer_cb->on_close = disconnect_peer;
  epoll_ctl(EPFD, EPOLL_CTL_ADD, peer_fd, &peer_cb->event);
  return peer_cb;
}

void init(char *port) {
  port = port ? port : "8080";
  NAME = strdup(port);
  int fd = listen1(port);
  log_info("LISTENER port [%s], fd [%d]\n", port, fd);
  epoll_cb *cb = alloc_cb(fd);
  cb->event.events = EPOLLIN;
  cb->on_EPOLLIN = accept_peer;
  epoll_ctl(EPFD, EPOLL_CTL_ADD, fd, &cb->event);

  peers = hash_new(128, hash_str, (int (*)(void *, void *))strcmp);

  timer(tick_ms, peer_tick, NULL);
  timer(2 * tick_ms + 113, peer_reconnect, NULL);
}

PD *get_pd(int fd, epoll_cb *cb) {
  PD *pd = (PD *)malloc(sizeof(PD));
  clear_pd(pd);
  pd->fd = fd;
  pd->cb = cb;
  return pd;
}

void clear_pd(PD *pd) {
  memset(pd, 0, sizeof(PD));
}