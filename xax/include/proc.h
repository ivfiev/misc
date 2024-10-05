#ifndef XAX_PROC_H
#define XAX_PROC_H

typedef struct {
  char name[128];
  size_t start;
  size_t size;
} mem_desc;

typedef struct {
  char *bytes;
  size_t base_addr;
  size_t size; // bytes count, arr may be larger.
} mem_block;

pid_t get_pid(char *);

size_t read_mem_desc(pid_t pid, mem_desc ds[], size_t max_len);

mem_desc *find_mem_desc(char *key, mem_desc ds[], size_t len);

int open_mem(pid_t pid);

mem_block *read_mem(int fd, size_t addr, size_t size);

size_t write_mem(int fd, size_t addr, char buf[], size_t size);

int close_mem(int fd);

void free_mem(mem_block *);

#endif
