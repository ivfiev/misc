#ifndef XAX_UTIL_H
#define XAX_UTIL_H

#define sizearr(arr) (sizeof(arr) / sizeof((arr)[0]))

void err_fatal(char *);
size_t run_cmd(char *cmd, char buf[], size_t max_len);
size_t strsplit(char *str, const char *sep, char **toks, size_t buf_len);

#endif
