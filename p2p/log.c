#include <stdarg.h>
#include "p2p.h"

extern int EPFD;
extern char *NAME;

int DEBUG_ENABLED;

struct log_data {
  int stdout_fd;
  int stderr_fd;
  int socket_fd;
  char *port;
  bool reconnect;
};

void on_error(epoll_cb *cb) {
  static int pipe_fd;
  static int stdout_fd;
  static int socket_fd;
  if (cb != NULL) {
    struct log_data *data = cb->data;
    pipe_fd = cb->fd;
    stdout_fd = data->stdout_fd;
    socket_fd = data->socket_fd;
  } else if (pipe_fd > 0) {
    // caught signal, flushing logs before aborting
    char buf[BUF_SIZE];
    ssize_t bytes = read(pipe_fd, buf, BUF_SIZE);
    write(stdout_fd, buf, bytes);
    if (socket_fd > 0) {
      send(socket_fd, buf, bytes, MSG_NOSIGNAL);
    }
  }
}

void on_print(epoll_cb *cb) {
  char buf[BUF_SIZE];
  struct log_data *data = cb->data;
  on_error(cb);
  ssize_t bytes = read(cb->fd, buf, BUF_SIZE);
  write(data->stdout_fd, buf, bytes);
  if (data->socket_fd < 0 && data->port != NULL && data->reconnect) {
    data->reconnect = false;
    log_info("streaming logs to [%s]", data->port);
    data->socket_fd = connect1(data->port);
  }
  if (data->socket_fd > 0) {
    ssize_t sent = send(data->socket_fd, buf, bytes, MSG_NOSIGNAL);
    if (sent < bytes) {
      log_info("logger socket closed");
      close(data->socket_fd);
      data->socket_fd = -1;
    }
  }
}

void reset_stdout(epoll_cb *cb) {
  struct log_data *data = cb->data;
  close(STDOUT_FILENO);
  close(STDERR_FILENO);
  dup2(data->stdout_fd, STDOUT_FILENO);
  dup2(data->stderr_fd, STDERR_FILENO);
  close(data->stdout_fd);
  close(data->stderr_fd);
  data->stdout_fd = -1;
  data->stderr_fd = -1;
  free(data->port);
  //setvbuf(stdout, NULL, _IOLBF, 0);
  log_info("reset stdout");
}

void reset_reconnect(epoll_cb *cb) {
  timer_data *timer_data = cb->data;
  struct log_data *log_data = timer_data->data;
  log_data->reconnect = true;
}

void init_log(char *port) {
  int stdout_fd = dup(STDOUT_FILENO);
  int stderr_fd = dup(STDERR_FILENO);
  int rw_fd[2];
  pipe(rw_fd);
  dup2(rw_fd[1], STDOUT_FILENO);
  dup2(rw_fd[1], STDERR_FILENO);
  close(rw_fd[1]);
  setvbuf(stdout, NULL, _IOLBF, 0);
  setvbuf(stderr, NULL, _IOLBF, 0);

  struct log_data *data = malloc(sizeof(struct log_data));
  data->stdout_fd = stdout_fd;
  data->stderr_fd = stderr_fd;
  data->socket_fd = -1;
  data->reconnect = true;
  data->port = strdup(port);

  epoll_cb *cb = alloc_cb(rw_fd[0]);
  cb->data = data;

  cb->event.events = EPOLLIN | EPOLLHUP | EPOLLERR;
  cb->on_EPOLLIN = on_print;
  cb->on_close = reset_stdout;
  epoll_ctl(EPFD, EPOLL_CTL_ADD, rw_fd[0], &cb->event);

  timer(5000, reset_reconnect, data);

  char *debug = getenv("DEBUG");
  DEBUG_ENABLED = debug != NULL && !strcmp(debug, "1");
}

void err_fatal(const char *format, ...) {
  TIMESTAMP(ts);
  printf("[%s ERROR] [node:%s] [%s]: ", ts, NAME, strerror(errno));
  va_list args;
  va_start(args, format);
  vfprintf(stdout, format, args);
  va_end(args);
  printf("\n");
  exit(1);
}

void err_info(const char *format, ...) {
  TIMESTAMP(ts);
  printf("[%s ERROR] [node:%s] [%s]: ", ts, NAME, strerror(errno));
  va_list args;
  va_start(args, format);
  vfprintf(stdout, format, args);
  va_end(args);
  printf("\n");
}

void log_debug(const char *format, ...) {
  TIMESTAMP(ts);
  if (DEBUG_ENABLED) {
    printf("[%s] [node:%s] ", ts, NAME);
    va_list args;
    va_start(args, format);
    vfprintf(stdout, format, args);
    va_end(args);
    printf("\n");
  }
}

void log_info(const char *format, ...) {
  TIMESTAMP(ts);
  printf("[%s] [node:%s] ", ts, NAME);
  va_list args;
  va_start(args, format);
  vfprintf(stdout, format, args);
  va_end(args);
  printf("\n");
}