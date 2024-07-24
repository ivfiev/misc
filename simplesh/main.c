#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/wait.h>
#include <ctype.h>

int main(void) {
  char buf[128];
  char *args[8];
  memset(args, 0, sizeof(args));
  for (;;) {
    printf(">> ");
    char *input = fgets(buf, sizeof(buf), stdin);
    if (input == NULL) {
      printf("failed to read input!\n");
      exit(1);
    }
    pid_t pid = fork();
    if (pid == -1) {
      printf("failed to fork!\n");
      exit(1);
    } else if (pid > 0) {
      wait(0);   
    } else {
      int len = strlen(input);
      for (int i = len - 1; 0 <= i && isspace(input[i]); i--) {
        input[i] = 0;
      }
      int i = 0;
      char *tok = strtok(input, " ");
      while (tok != NULL && i < sizeof(args) / sizeof(char *)) {
        args[i++] = tok;
        tok = strtok(NULL, " ");
      }
      int err = execvp(args[0], args); 
      if (err == -1) {
        printf("failed to exec!\n");
        exit(1);
      }
    } 
  }
  return 0;
}
