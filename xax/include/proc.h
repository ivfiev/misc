#ifndef XAX_PROC_H
#define XAX_PROC_H

typedef struct {
  char name[128];
  size_t start;
  size_t length;
} mem_map;

pid_t get_pid(char *);
size_t read_mem_maps(pid_t pid, mem_map maps[], size_t max_maps);

#endif
