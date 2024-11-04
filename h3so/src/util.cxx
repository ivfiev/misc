#include <stdarg.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include "h3so.hxx"
#include <stdlib.h>
#include <sys/time.h>
#include <time.h>

#define LOG_FILE "/home/fi/dev/misc/h3so/h3sod.log"

void timestamp(char *buf) {
  struct timeval tv;
  struct tm *tm_info;
  gettimeofday(&tv, NULL);
  tm_info = localtime(&tv.tv_sec);
  size_t c = strftime(buf, 12, "%H:%M:%S", tm_info);
  snprintf(buf + c, 19, ".%03ld", tv.tv_usec / 1000);
}

void log(const char *format, ...) {
  char ts[32];
  FILE *log_file = fopen(LOG_FILE, "a+");
  if (log_file == nullptr) {
    exit(91);
  }
  timestamp(ts);
  va_list args;
  va_start(args, format);
  fprintf(log_file, "[%s] ", ts);
  vfprintf(log_file, format, args);
  fprintf(log_file, "\n");
  va_end(args);
  fclose(log_file);
}