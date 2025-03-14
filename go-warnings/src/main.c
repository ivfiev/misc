#include <stdio.h>
#include <stdlib.h>
#include <sys/ptrace.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/user.h>
#include <unistd.h>
#include <errno.h>

void handle_syscall(pid_t pid) {
  struct user_regs_struct regs;
  ptrace(PTRACE_GETREGS, pid, 0, &regs);
  printf("[+] Syscall %lld intercepted (RAX=%lld)\n", regs.orig_rax, regs.rax);

  if (regs.orig_rax == 1) {  // execve syscall on x86_64
      printf("intercepted write(2)");
      // regs.rdi = (unsigned long)"/bin/echo";  // Change execve to /bin/echo
      // ptrace(PTRACE_SETREGS, pid, 0, &regs);
  }
}

void trace_process(pid_t child) {
  int status;
  while (1) {
      waitpid(child, &status, 0);
      if (WIFEXITED(status)) break;

      if (WSTOPSIG(status) == SIGTRAP) {
          handle_syscall(child);
      }

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
      ptrace(PTRACE_SETOPTIONS, child, 0,
            PTRACE_O_TRACEFORK | PTRACE_O_TRACEEXEC | PTRACE_O_TRACECLONE);
      trace_process(child);
  }

return 0;
}