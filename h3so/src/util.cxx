#include <stdarg.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include "h3so.hxx"

#define LOG_FILE "/dev/shm/h3sod.log"

void log(const char *format, ...) {
  FILE *log_file = fopen(LOG_FILE, "a+");
  if (log_file == nullptr) {
    perror(strerror(errno));
  }
  va_list args;
  va_start(args, format);
  vfprintf(log_file, format, args);
  fprintf(log_file, "\n");
  va_end(args);
  fclose(log_file);
}