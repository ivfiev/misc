#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <ctype.h>
#include <signal.h>
#define SAMPLES_PER_SEC 10
#define SAMPLES_RING 50

int get_cpus(void) {
  char buf[1024];
  char *line;
  FILE *fp;

  fp = popen("lscpu", "r"); 
  if (fp == NULL) {
    printf("failed to run lscpu\n");
    exit(1);
  }

  for (;;) {
    char *s = fgets(buf, sizeof(buf), fp);
    if (s == NULL) {
      printf("failed to read lscpu output\n");
      exit(1);
    }

    line = strcasestr(s, "CPU(s)");
    if (line != NULL) {
      break;
    }
  }

  while (*line && !isdigit(*line)) {
    line++;
  }

  if (!*line) {
    printf("failed to parse the number of CPUs\n");
    exit(1);
  }

  pclose(fp);
  return atoi(line);
}

char *read_cpuinfo(void) {
  const int size = 64 * 1024;
  char *buf = calloc(size, sizeof(char));
  int fp, bytes, len;

  len = 0;
  fp = open("/proc/cpuinfo", O_RDONLY);
  while ((bytes = read(fp, buf + len, size - 1 - len)) > 0) {
    len += bytes;
  }

  if (bytes < 0) {
    printf("failed to read cpuinfo\n");
    exit(1);
  }

  close(fp);
  return buf;
}

void index_cpuinfo(int indexes[], int cpus) {
  char *cpuinfo = read_cpuinfo();
  char *ix = cpuinfo;
  int cpu = 0;

  while (cpu < cpus) {
    ix = strstr(ix, "cpu MHz");
    if (ix == NULL) {
      printf("failed to index cpuinfo\n");
      exit(1);
    }
    indexes[cpu++] = ix - cpuinfo;
    ix++;
  }

  free(cpuinfo);
}

void read_clocks(int clocks[], int indexes[], int cpus) {
  char *cpuinfo = read_cpuinfo();

  for (int i = 0; i < cpus; i++) {
    char *ix = cpuinfo + indexes[i] - cpus;
    ix = strstr(ix, "MHz");

    if (ix == NULL) {
      printf("failed to parse indexed cpuinfo\n");
      exit(1);
    }
    while (!isdigit(*ix)) {
      ix++;
    }
    clocks[i] = atoi(ix);
  }

  free(cpuinfo);
}

void cleanup(int) {
  (void)system("clear");
  exit(0);
}

void handle_sigint(void) {
  struct sigaction a;
  memset(&a, 0, sizeof(a));
  a.sa_handler = cleanup;
  if (sigaction(SIGINT, &a, NULL) < 0) {
    printf("signal handler failed\n");
  }
}

int main(void) {
  handle_sigint();
  int cpus = get_cpus(), samples = 0;
  int indexes[cpus], clocks[cpus], maxes[cpus], ring[cpus][SAMPLES_RING];
  float avgs[cpus];

  memset(avgs, 0, cpus * sizeof(float));
  memset(maxes, 0, cpus * sizeof(int));
  memset(ring, 0, cpus * SAMPLES_RING * sizeof(int));
  index_cpuinfo(indexes, cpus);

  for (;;) {
    for (int i = 0; i < SAMPLES_PER_SEC; i++) {
      read_clocks(clocks, indexes, cpus);
      
      for (int cpu = 0; cpu < cpus; cpu++) {
        maxes[cpu] = maxes[cpu] < clocks[cpu] ? clocks[cpu] : maxes[cpu];
        avgs[cpu] = (avgs[cpu] * samples + clocks[cpu]) / (samples + 1);
        ring[cpu][samples % SAMPLES_RING] = clocks[cpu];
      }

      samples++;
      usleep(1000000 / SAMPLES_PER_SEC);
    }
    
    printf("\e[1;1H\e[2J");
    printf("core#\tnow\tmax(%d)\tavg(%d)\tmax(*)\tavg(*)\n", SAMPLES_RING, SAMPLES_RING);

    for (int i = 0; i < cpus; i++) {

      int max = 0;
      float avg = 0;
      for (int j = 0; j < SAMPLES_RING; j++) {
        max = ring[i][j] > max ? ring[i][j] : max;
        avg += ring[i][j];
      }
      avg /= SAMPLES_RING;

      printf("%d\t%d\t%d\t%d\t%d\t%d\n", i, clocks[i], max, (int)avg, maxes[i], (int)avgs[i]);
    }
  }

  return 0;
}
