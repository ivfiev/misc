#include "p2p.h"

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

