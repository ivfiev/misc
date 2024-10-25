#include <string.h>
#include <stdlib.h>
#include "csrh.h"

struct history *history_new() {
  struct history *h = malloc(sizeof(struct history));
  memset(h, 0, sizeof(struct history));
  return h;
}

int history_change(struct history *h, float x, float y) {
  if (!FLOAT_EQ(h->xs[h->i], x) || !FLOAT_EQ(h->ys[h->i], y)) {
    h->count = MIN(HISTORY_LEN, h->count + 1);
    h->xs[h->i] = x;
    h->ys[h->i] = y;
    h->i++;
    h->i %= HISTORY_LEN;
    return 1;
  }
  return 0;
}

static int coord_legit(float xy) {
  return IN_RANGE(-5000, xy, 5000) && !FLOAT_EQ(xy, 0) && !is_div_by(xy, 0.000250);
}

int history_legit(struct history *h) {
  if (h->count != HISTORY_LEN) {
    return 0;
  }
  float total = 0;
  for (int i = h->i;; i = (i + 1) % HISTORY_LEN) {
    int j = (i + 1) % HISTORY_LEN;
    if (j == h->i) {
      break;
    }
    float d = dist(h->xs[i], h->ys[i], h->xs[j], h->ys[j]);
    total += d;
    if (d > 5 * MAX_SPEED / TICKS_PER_SEC) {
      return 0;
    }
    if (!coord_legit(h->xs[i]) || !coord_legit(h->ys[i])) {
      return 0;
    }
  }
  return total > (MAX_SPEED / TICKS_PER_SEC) * HISTORY_LEN / 25.0;
}
