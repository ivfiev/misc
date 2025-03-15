#include <stdio.h>
#include <stdlib.h>
#include <sys/ptrace.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/user.h>
#include <unistd.h>
#include <errno.h>
#include "util.h"
#include "proc.h"

void handle_syscall(pid_t pid) {
  struct user_regs_struct regs;
  ptrace(PTRACE_GETREGS, pid, 0, &regs);
  if (regs.orig_rax == 59) { 
    // execve(2)
    if (proc_get(pid)) {
      return;
    }
    void *str = (void *)regs.rdi;
    char filename[1024];
    ptrace_read(pid, str, (uint8_t *)filename, sizeof(filename));
    proc_t *proc = proc_new(pid, filename);
    proc_add(proc);
    proc_print(proc);
  }
}

void trace_process(pid_t process) {
  int status;
  while (1) {
    pid_t child = waitpid(-1, &status, 0);
    if (WIFEXITED(status)) {
      if (child == process) {
        break;
      } else {
        continue;
      }
    }
    handle_syscall(child);
    ptrace(PTRACE_SYSCALL, child, 0, 0);
  }
}

int main(int argc, char **argv) {
  if (argc < 2) {
    fprintf(stderr, "Usage: %s /path/to/program\n", argv[0]);
    return 1;
  }
  pid_t child = fork();
  if (child == 0) {
    ptrace(PTRACE_TRACEME, 0, 0, 0);
    return execvp(argv[1], &argv[1]);
  } else {
    waitpid(child, NULL, 0);
    ptrace(PTRACE_SETOPTIONS, child, 0, PTRACE_O_TRACEFORK | PTRACE_O_TRACEVFORK | PTRACE_O_TRACECLONE);
    ptrace(PTRACE_SYSCALL, child, 0, 0);  
    trace_process(child);
  }

return 0;
}