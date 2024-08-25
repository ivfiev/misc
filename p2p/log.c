#include <stdarg.h>
#include "p2p.h"

extern int EPFD;
extern char *NAME;

struct log_data {
  int stdout_fd;
  int log_fd;
  char *port;
  bool reconnect;
};

void on_print(epoll_cb *cb) {
  char buf[BUF_SIZE];
  struct log_data *data = cb->data;
  ssize_t bytes = read(cb->fd, buf, BUF_SIZE);
  write(data->stdout_fd, buf, bytes);
  if (data->log_fd < 0 && data->port != NULL && data->reconnect) {
    data->reconnect = false;
    log_info("streaming logs to [%s]\n", data->port);
    data->log_fd = connect1(data->port);
  }
  if (data->log_fd > 0) {
    ssize_t sent = send(data->log_fd, buf, bytes, MSG_NOSIGNAL);
    if (sent < bytes) {
      log_info("logger socket closed\n");
      close(data->log_fd);
      data->log_fd = -1;
    }
  }
}

void reset_stdout(epoll_cb *cb) {
  struct log_data *data = cb->data;
  close(STDOUT_FILENO);
  dup2(data->stdout_fd, STDOUT_FILENO);
  close(data->stdout_fd);
  data->stdout_fd = -1;
  free(data->port);
  //setvbuf(stdout, NULL, _IOLBF, 0);
  log_info("reset stdout\n");
}

void reset_reconnect(epoll_cb *cb) {
  timer_data *timer_data = cb->data;
  struct log_data *log_data = timer_data->data;
  log_data->reconnect = true;
}

void init_log(char *port) {
  int stdout_fd = dup(STDOUT_FILENO);
  int rw_fd[2];
  pipe(rw_fd);
  dup2(rw_fd[1], STDOUT_FILENO);
  close(rw_fd[1]);
  setvbuf(stdout, NULL, _IOLBF, 0);

  struct log_data *data = malloc(sizeof(struct log_data));
  data->stdout_fd = stdout_fd;
  data->log_fd = -1;
  data->reconnect = true;
  data->port = strdup(port);

  epoll_cb *cb = alloc_cb(rw_fd[0]);
  cb->data = data;

  cb->event.events = EPOLLIN | EPOLLHUP | EPOLLERR;
  cb->on_EPOLLIN = on_print;
  cb->on_close = reset_stdout;
  epoll_ctl(EPFD, EPOLL_CTL_ADD, rw_fd[0], &cb->event);

  timer(5000, reset_reconnect, data);
}

void err_fatal(const char *msg) {
  printf("%s ***> %s\n", NAME, msg);
  printf("%s ***> %s\n", NAME, strerror(errno));
  exit(1);
}

void err_info(const char *msg) {
  printf("%s ***> %s\n", NAME, msg);
  printf("%s ***> %s\n", NAME, strerror(errno));
}

void log_debug(const char *format, ...) {
  char *debug = getenv("DEBUG");
  if (debug != NULL && !strcmp(debug, "1")) {
    printf("%s -> ", NAME);
    va_list args;
    va_start(args, format);
    vfprintf(stdout, format, args);
    va_end(args);
  }
}

void log_info(const char *format, ...) {
  printf("%s -> ", NAME);
  va_list args;
  va_start(args, format);
  vfprintf(stdout, format, args);
  va_end(args);
}