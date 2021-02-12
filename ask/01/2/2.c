#include <stdio.h>
#include <stdint.h>
#include <limits.h>
#define eprintf(...) fprintf(stderr, __VA_ARGS__)


static const int M[5] = {
	0x55555555,
	0x33333333,
	0x0F0F0F0F,
	0x00FF00FF,
	0x0000FFFF
};

uint32_t popcnt(uint32_t x) {
	x = ((x >> (1 << 0)) & M[0]) + (x & M[0]);
	x = ((x >> (1 << 1)) & M[1]) + (x & M[1]);
	x = ((x >> (1 << 2)) + x) & M[2];
	x = ((x >> (1 << 3)) + x) & M[3];
	x = ((x >> (1 << 4)) + x) & M[4];
	return x;
}

void test(uint32_t x) {
	printf("popcnt(%u) = %u\n", x, popcnt(x));
}

int main() {
	for (int i = 0; ; i++) {
		if (popcnt(i) != __builtin_popcount(i)) {
			printf("error for %u\n", i);
		}
		if (i == INT_MAX) break;
	}
	return 0;
}
