#include <stdio.h>
#include <stdint.h>
#include <limits.h>
#define eprintf(...) fprintf(stderr, __VA_ARGS__)

int32_t mysgn(int32_t x) {
	return (x >> 31) + ((-x & ~x) >> 31 & 1);
}

int32_t sgn(int32_t x) {
	if (x > 0) return 1;
	if (x == 0) return 0;
	return -1;
}

int main() {
	for (int i = INT_MIN; ; i++) {
		if (mysgn(i) != sgn(i))
			printf("%d\n", i);
		if (i == INT_MAX) break;
	}
	return 0;
}
