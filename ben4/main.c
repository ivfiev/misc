#include <stdio.h>
#include <unistd.h>
#include <pthread.h>
#include <string.h>
#include <stdatomic.h>
#include <stdlib.h>
#include <math.h>

volatile int FINISH;
atomic_int EVENTS;

void *sleep10(void *arg) {
  sleep(10);
  FINISH = 1;
  return NULL;
}

void *work(void *arg) {
  float primes[1229];
  float recips[1229];
  float square[1229];
  int k;
  int count = 0;
  for (count = 0; !FINISH; count++) {
    memset(primes, 0, sizeof(primes));
    primes[0] = 2;
    recips[0] = 0.5;
    square[0] = 4;
    k = 1;
    for (float n = 3; n < 10000; n += 2) {
      for (int i = 0; i < k; i++) {
        if (rintf(n * recips[i]) * primes[i] == n) {
          break;
        }
        if (square[i] > n) {
          primes[k] = n;
          recips[k] = 1 / n;
          square[k] = n * n;
          k++;
          break;
        }
      }
    }
  }
  printf("%d\n", k);
  printf("%d\n", count);
  atomic_fetch_add(&EVENTS, count);
  return NULL;
}

int main(int argc, char **argv) {
  int threads = atoi(argv[1]);
  pthread_t id;
  pthread_t workers[threads];

  pthread_create(&id, NULL, sleep10, NULL);
  for (int i = 0; i < threads; i++) {
    pthread_create(workers + i, NULL, work, NULL);
  }

  pthread_join(id, NULL);
  for (int i = 0; i < threads; i++) {
    pthread_join(workers[i], NULL);
  }

  printf("Events: %d\n", EVENTS);
  return 0;
}
