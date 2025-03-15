#include <stdio.h>
#include <stdlib.h>
#include <sys/ptrace.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/user.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include "util.h"
#include "proc.h"

void handle_syscall(pid_t pid) {
  struct user_regs_struct regs;
  ptrace(PTRACE_GETREGS, pid, 0, &regs);
  proc_t *proc = proc_get(pid);
  if (regs.orig_rax == 59) {
    // execve(2)
    if (regs.rax == -ENOSYS) {
      // entering execve
      void *str = (void *)regs.rdi;
      char filename[1024];
      ptrace_read(pid, str, (uint8_t *)filename, sizeof(filename));
      proc_t *proc = proc_new(pid, filename);
      proc_add(proc);
    } else {
      // exiting execve
      if (strstr(proc->filename, "compile")) {
        //proc_print(proc);
        uint8_t bytes[8];
        void *addr = (void *)0xcd3f19;
        ptrace_read(pid, addr, bytes, 8);
        // for (int i = 0; i < 8; i++) {
        //   printf("0x%lx ", bytes[i]);
        // }
        // puts("");
        bytes[0] = 0x90;
        bytes[1] = 0x90;
        bytes[2] = 0x90;
        bytes[3] = 0x90;
        bytes[4] = 0x90;
        ptrace_write(pid, addr, bytes, 8);
        // ptrace_read(pid, addr, bytes, 8);
        // for (int i = 0; i < 8; i++) {
        //   printf("0x%lx ", bytes[i]);
        // }
        // puts("");
        // for (;;) {}
      }
    }
  }
  // if (regs.orig_rax == 59 && proc == NULL && regs.rax != -ENOSYS) { 
  //   // execve(2)
  //   void *str = (void *)regs.rdi;
  //   char filename[1024];
  //   ptrace_read(pid, str, (uint8_t *)filename, sizeof(filename));
  //   proc_t *proc = proc_new(pid, filename);
  //   proc_add(proc);
  //   proc_print(proc);
  //   proc->bp_count++;
  //   for (;;) {}
  // }
  // if (proc != NULL && proc->bp_count > 0) {
  //   proc_print(proc);
  //   for (;;) {}
  // }
}

void trace_tree(pid_t root) {
  int status;
  for (;;) {
    pid_t child = waitpid(-1, &status, 0);
    if (WIFEXITED(status)) {
      if (child == root) {
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
    trace_tree(child);
  }
  return 0;
}