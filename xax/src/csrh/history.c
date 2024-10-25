#include <string.h>
#include <stdlib.h>
#include "csrh.h"
#include "util.h"

#define READ_COORDS(h, count) \
  float coords[(count) * 2]; \
  size_t coords_count = history_read(h, coords, count) \


struct history *history_new() {
  struct history *h = malloc(sizeof(struct history));
  memset(h, 0, sizeof(struct history));
  return h;
}

static int history_head_ix(struct history *h) {
  int ix = h->i - 1;
  if (h->count < HISTORY_LEN) {
    return ix;
  }
  return WRAP_IX(ix, HISTORY_LEN);
}

static size_t history_read(struct history *h, float coords[], size_t count) {
  int hi = history_head_ix(h);
  if (hi < 0) {
    return 0;
  }
  for (int i = 0; i < MIN(count, h->count) * 2; i += 2) {
    coords[i] = h->xs[hi];
    coords[i + 1] = h->ys[hi];
    hi = WRAP_IX(hi, HISTORY_LEN);
  }
  return MIN(count, h->count);
}

static size_t history_write(struct history *h, float x, float y) {
  h->count = MIN(HISTORY_LEN, h->count + 1);
  h->xs[h->i] = x;
  h->ys[h->i] = y;
  h->i++;
  h->i %= HISTORY_LEN;
  return h->count;
}

int history_change(struct history *h, float x, float y) {
  READ_COORDS(h, 1);
  if (coords_count == 0 || !FLOAT_EQ(coords[0], x) || !FLOAT_EQ(coords[1], y)) {
    history_write(h, x, y);
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
