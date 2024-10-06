#ifndef XAX_UTIL_H
#define XAX_UTIL_H

#define SIZEARR(arr) (sizeof(arr) / sizeof((arr)[0]))
#define MIN(x, y) ((x) < (y) ? (x) : (y))
#define MAX(x, y) ((x) > (y) ? (x) : (y))

void err_fatal(char *);

size_t read_all(int fd, char buf[], size_t size);

size_t run_cmd(char *cmd, char buf[], size_t size);

size_t strsplit(char *str, const char *sep, char **toks, size_t size);

#endif
