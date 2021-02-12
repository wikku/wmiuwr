#include <stdio.h>
#include <stdint.h>
#include <limits.h>
#define eprintf(...) fprintf(stderr, __VA_ARGS__)

struct T {
	long min, max, avg;
};

struct T puzzle8(long *a /* %rsi */, long n /* %rdx */) {
	/* mamy ukryty pierwszy argument w %rdi - wskaźnik, gdzie umieścić wynik */
	long max /* %r8 */ = LONG_MIN;
	long min /* %r9 */ = LONG_MAX;
	long sum /* %rax */ = 0;
	for (long i /* %r10d */ = 0; i < n; i++) {
		long ai = a[i];
		sum += ai;
		if (ai < min) min = ai;
		if (ai > max) max = ai;
	}
	struct T result = { min, max, sum / n };
	return result;
}

// wywnioskować moglibyśmy
// void* puzzle8(void*, uint64_t*, uint64_t)

int main() {
	return 0;
}
