#include <stdio.h>
#include <stdint.h>
#include <alloca.h>
#define eprintf(...) fprintf(stderr, __VA_ARGS__)

long aframe(long n, long idx, long *q) {
	long i;
	long **p = alloca(n * sizeof(long *));
	p[n-1] = &i;
	for (i = 0; i < n; i++)
		p[i] = q;
	return *p[idx];
}

int main() {
	return 0;
}
