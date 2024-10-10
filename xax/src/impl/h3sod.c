#include <stdio.h>
#include <unistd.h>
#include "args.h"
#include "proc.h"
#include "util.h"
#include "scan.h"
#include "hashtable.h"

static void run(void) {
  // 285 0x11e8c70 0x308c70 15         x coord
  //
}

__attribute__((constructor))
static void init(void) {
  args_add("h3sod", run);
}
