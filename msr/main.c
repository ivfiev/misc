#include <stdio.h>
#include <fcntl.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdint.h>

#define OFFSET_POWER_UNIT 0xc0010299
#define OFFSET_CORE_ENERGY 0xc001029a
#define OFFSET_PACKAGE_ENERGY 0xc001029b

#define CPUS 32

static float ENERGY_UNIT;

static int open_msr(int cpu) {
	char str[32];
	snprintf(str, sizeof(str), "/dev/cpu/%d/msr", cpu);
	int fd = open(str, O_RDONLY);
	if (fd < 0) {
		perror("could not open msr");
		exit(-2);
	}
	return fd;
}

static uint64_t read_msr(int fd, __off64_t offset) {
	uint8_t buf[8];
	int read = pread(fd, buf, sizeof(buf), offset);
	if (read != 8) {
		perror("could not read msr");
		exit(-1);
	}
	return *(uint64_t *)buf;
}

int main(void) {
	printf("%.1f\n", 1.81);
	int fds[CPUS];
	uint64_t pkg0 = 0, pkg1 = 0;
	uint64_t cpu0[CPUS] = {0}, cpu1[CPUS] = {0};
	for (int i = 0; i < CPUS; i++) {
		fds[i] = open_msr(i);
	}
	uint64_t unit = read_msr(fds[0], OFFSET_POWER_UNIT);
	ENERGY_UNIT = 1.0 / (1 << ((unit >> 8) & 0x1F));
	
	for (;;) {

		// package
		pkg0 = read_msr(fds[0], OFFSET_PACKAGE_ENERGY);
		if (pkg1) {
			uint64_t delta = pkg0 - pkg1;
			float watts = ENERGY_UNIT * delta;
			printf("package power: [%f]\n", watts);
		}
		pkg1 = pkg0;

		// per-core
		for (int i = 0; i < CPUS; i++) {
			if (i == CPUS / 2) puts("");
			cpu0[i] = read_msr(fds[i], OFFSET_CORE_ENERGY);
			if (cpu1[i]) {
				uint64_t delta = cpu0[i] - cpu1[i];
				float watts = ENERGY_UNIT * delta;
				printf("[%f], ", watts);
			}
			cpu1[i] = cpu0[i];
		}

		puts("");
		sleep(1);
	}
}