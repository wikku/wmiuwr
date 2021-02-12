#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <limits.h>
#define eprintf(...) fprintf(stderr, __VA_ARGS__)

#define TEST_ALL(p) do { \
	int32_t x = INT_MIN; \
	for (;;) { \
		if (!(p)) printf("%d\n", x); \
		if (x == INT_MAX) break; \
		x += 1; \
	} \
	} while (0)


int main() {
	int32_t y = 2;
	TEST_ALL(x * ~y + (uint32_t)y * (uint32_t)x == -x);
	/*
	int32_t p = 1 << 29;
	for (int i = 0-16; i < 8-16; i++) {
		printf("%d << 29 = %d\n", i, i << 29);
		printf("%d * p = %d\n", i, i * p);
	}
	*/

	return 0;
}
