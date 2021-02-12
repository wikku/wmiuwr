#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <math.h>
#define eprintf(...) fprintf(stderr, __VA_ARGS__)

uint32_t negatef(uint32_t x) {
	// przestawiamy bit znaku
	return 0x80000000 ^ x;
}

int32_t log2absf(uint32_t x) {
	//         usuwamy znak
	//         przesuwamy exponent
	//                    odejmujemy bias
	return ((x & 0x7FFFFFFF) >> 23) - 127;
}

bool equalf(uint32_t x, uint32_t y) {
	// Jedyny przypadek, gdy zachodzi równość przy
	// różnych reprezentacjach to -0 i +0.
	return (x == y) | ((x ^ y) == 0x80000000);
}

bool cmpf(uint32_t x, uint32_t y) {
	uint32_t xlty = (((x - y) ^ ((x ^ y) & ((x - y) ^ x))) >> 31) & 1;
	return xlty & ~((x == 0x80000000) & (y == 0));
}

typedef union {
	uint32_t i;
	float f;
} U;

int32_t roundi(double f) {
	f = floor(f);
	return (int32_t) (f < 0 ? (f - 0.5) : (f + 0.5));
}

int main() {
	U u;
	for (u.f = 9999999999; ; u.i++) {
		if (!(isnormal(u.f))) continue;
		if (isnan(u.f)) continue;
		//U u2 = {.f = -u.f};
		//if (negatef(u.i) != u2.i) printf("%d\n", u.i);
		//if (log2absf(u.i) != roundi(log2(fabs(u.f)))) printf("%d\n", u.i);
		if (u.i == 0xFFFFFFFF) break;
	}
	return 0;
}
