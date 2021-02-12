#include <stdio.h>
#include <stdint.h>
#define eprintf(...) fprintf(stderr, __VA_ARGS__)

long puzzle(long n, long *p) {
	long result = 0;
	long tmp;
	if (n > 0) {
		result = puzzle(2*n, &tmp);
		result += tmp;
		n += result;
	}
	*p = n;
	return result;
}

int main() {
	return 0;
}
