#include <stdio.h>
#include <stdint.h>
#include <limits.h>
#include <math.h>
#define eprintf(...) fprintf(stderr, __VA_ARGS__)

int32_t threefourths(int32_t x) {
	return (x >> 2) + (x >> 1) + (x & x >> 1 & 1);
}

int main() {
	/*
	printf("%d\n", threefourths(0));
	printf("%d\n", threefourths(1));
	printf("%d\n", threefourths(2));
	printf("%d\n", threefourths(3));
	printf("%d\n", threefourths(4));
	*/
	for (int32_t i = INT_MIN; ; i++) {
		if (threefourths(i) != (int32_t) floor(3.0*i/4.0))
			printf("%d\n", i);
		if (i == INT_MAX) break;
	}

	return 0;
}
