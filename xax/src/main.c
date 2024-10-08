#include "args.h"

int main(int argc, char **argv) {
  args_parse(argc - 2, argv + 2);
  args_exec(argv[1]);
  return 0;
}
