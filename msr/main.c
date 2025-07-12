#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdint.h>

#define OFFSET_CORE_ENERGY 0xc001029a
#define OFFSET_PACKAGE_ENERGY 0xc001029b

#define ENERGY_UNIT (15.3 / 1000000)

int main(void) {
	int fd = open("/dev/cpu/0/msr", O_RDONLY);
	if (fd < 0) {
		perror("could not open msr");
		return -1;
	}
	uint8_t buf[8];
	uint64_t previous = 0;
	for (;;) {
		int read = pread(fd, buf, 8, OFFSET_PACKAGE_ENERGY);
		if (read != 8) {
			perror("could not read msr");
			return -2;
		}
		uint64_t units = *(uint64_t *)buf;
		if (previous) {
			float watts = (units - previous) * ENERGY_UNIT;
			printf("raw units: [0x%lx], watts: [%f]\n", units, watts);
		}
		previous = units;
		sleep(1);
	}
}