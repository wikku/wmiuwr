#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <limits.h>
#define eprintf(...) fprintf(stderr, __VA_ARGS__)

// mine
bool sum_overflows(int32_t x, int32_t y) {
	int32_t s = x + y;
	bool sltx = (uint32_t) (s-x ^ (s ^ x) & (s-x ^ s)) >> 31;
	bool ylt0 = (uint32_t) y >> 31;
	return sltx != ylt0;
}

// hacker's delight
bool sum_overflows2(int32_t x, int32_t y) {
	return ((x+y ^ x) & (x+y ^ y)) >> 31 & 1;
}

#define test(x, y) do { printf("%d %d\n", sum_overflows(x, y), sum_overflows2(x, y)); } while (0)
int main() {
	test(0, 0);
	test(0, INT_MAX);
	test(1, INT_MAX);
	test(INT_MAX, INT_MAX);
	test(INT_MIN, INT_MAX);
	test(INT_MIN, -1);
	test(INT_MIN, INT_MIN);
	return 0;
}
