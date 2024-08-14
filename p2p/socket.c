#include "p2p.h"

int peer_listen(const char *port) {
  struct addrinfo hints;
  memset(&hints, 0, sizeof(hints));
  hints.ai_family = AF_INET;
  hints.ai_socktype = SOCK_STREAM;
  hints.ai_flags = AI_PASSIVE;
  struct addrinfo *bind_addr;
  if (getaddrinfo(0, port, &hints, &bind_addr) < 0) {
    err("getaddrinfo");
  }
  int fd = socket(hints.ai_family, hints.ai_socktype, 0);
  if (fd < 0) {
    err("peer_listen");
  }
  if (bind(fd, bind_addr->ai_addr, bind_addr->ai_addrlen) < 0) {
    err("bind");
  }
  if (listen(fd, 0) < 0) {
    err("listen");
  }
  freeaddrinfo(bind_addr);
  return fd;
}

int peer_connect(const char *port) {
  struct addrinfo hints;
  memset(&hints, 0, sizeof(hints));
  hints.ai_family = AF_INET;
  hints.ai_socktype = SOCK_STREAM;
  struct addrinfo *peer_addr;
  if (getaddrinfo(0, port, &hints, &peer_addr) < 0) {
    err("getaddrinfo");
  }
  int fd = socket(hints.ai_family, hints.ai_socktype, 0);

  // TODO - use non-blocking connects & handle conn errors in evt loop!
  if (connect(fd, peer_addr->ai_addr, peer_addr->ai_addrlen)) {
    err("connect");
  }
  freeaddrinfo(peer_addr);
  return fd;
}