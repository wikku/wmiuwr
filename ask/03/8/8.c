#include <stdio.h>
#include <stdint.h>
#include <limits.h>
#define eprintf(...) fprintf(stderr, __VA_ARGS__)

int32_t float2int(int32_t f) {
	int32_t  s = f >> 31;               /* -1 jeśli znak był ujemny */
	int32_t  e = (f >> 23 & 255) - 127; /* wykładnik po odjęciu bias */
	/* mantysa 1.xxx... dosunięta do lewej */
	uint32_t m = (uint32_t) f << 8 | 0x80000000;
	if (e > 31) return 0x80000000;
	if (e < 0) return 0;
	uint32_t result = (m >> (31 - e));
	return (result ^ s) + (s & 1);
}

int main() {
	for (int i = INT_MIN; ; i++) {
		float f = (float) i;
		uint32_t u = * (uint32_t*) &f;
		if (float2int(u) != (int32_t) f) printf("%d %d %d\n", i, float2int(u), (int32_t) f);
		if (i == INT_MAX) break;
	}
	return 0;
}
