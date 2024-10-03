#include <stdio.h>
#include <unistd.h>
#include <bits/pthreadtypes.h>
#include <pthread.h>
#include <string.h>
#include <stdatomic.h>
#include <stdlib.h>

#define number int

volatile int FINISH;
atomic_int EVENTS;

void *sleep10(void *arg) {
  sleep(10);
  FINISH = 1;
  return NULL;
}

void *work(void *arg) {
  number primes[4096];
  int k;
  int count = 0;
  for (count = 0; !FINISH; count++) {
    memset(primes, 0, sizeof(primes));
    k = 0;
    primes[k++] = 2;
    for (number n = 3; n < 10000; n += 2) {
      for (int i = 0; i < k; i++) {
        if (n % primes[i] == 0) {
          break;
        }
        if (primes[i] * primes[i] > n) {
          primes[k++] = n;
          break;
        }
      }
    }
  }
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
