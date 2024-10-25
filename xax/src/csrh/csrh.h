#ifndef XAX_CSRH_H
#define XAX_CSRH_H

#include "util.h"

#define BLOCKS 512
#define HISTORY_LEN 8
#define TICKS_PER_SEC 10
#define MAX_SPEED (250.0 + 2.5)

struct history {
  float xs[HISTORY_LEN];
  float ys[HISTORY_LEN];
  int i;
  int count;
};

struct history *history_new(void);

int history_change(struct history *h, float x, float y);

int history_legit(struct history *h);

#endif
