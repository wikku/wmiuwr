#include <stdio.h>
#include <stdint.h>
#define eprintf(...) fprintf(stderr, __VA_ARGS__)

// funkcja sprawdza czy wystąpił overflow przy dodawaniu
long decode(long x, long y) {
	long sum = x + y;
	x ^= sum;
	y ^= sum;
	return (unsigned long) (x & y) >> 63;
}

int main() {
	return 0;
}
