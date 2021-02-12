#include <stdio.h>
#include <stdint.h>
#include <limits.h>
#define eprintf(...) fprintf(stderr, __VA_ARGS__)

int32_t myabs(int32_t x) {
	int32_t s = x >> 31;
	return (s & -x) + (~s & x);
}

int main() {
	for (int i = INT_MIN; ; i++) {
		if (myabs(i) != abs(i))
			printf("%d\n", i);
		if (i == INT_MAX) break;
	}
	return 0;
}
