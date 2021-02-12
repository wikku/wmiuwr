#include <stdio.h>
#include <stdint.h>
#define eprintf(...) fprintf(stderr, __VA_ARGS__)

// funkcja liczy ile spośród pierwszych n bitów x jest zapalonych
int puzzle(long x /* %rdi */, unsigned n /* %esi */) {
	unsigned result = 0 /* %eax */;
	for (unsigned i = 0 /* %edx */; i < n; i++) {
		result += (x & 1);
		x >>= 1;
	}
	return result;
}

int main() {
	return 0;
}
