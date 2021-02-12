#include <stdio.h>
#include <stdint.h>
#include <limits.h>
#define eprintf(...) fprintf(stderr, __VA_ARGS__)

int32_t int2float(uint32_t i) {
	if (i == 0) return 0;
	if (i == 0x80000000) return 0x80000000 ^ ((127+31) << 23);
	uint32_t sgn = i & 0x80000000;
	if (sgn) i *= -1;
	uint32_t clz = __builtin_clz(i);
	uint32_t log2i = 31 - clz; // inaczej: pozycja wiodącej jedynki (0-indexed)
	uint32_t exp = (127 + log2i) << 23;
	i ^= (1 << log2i); // usuwamy wiodącą jedynkę (we floacie jest niejawna)
	int32_t sh = log2i - 23; // ile trzeba przesunac w prawo
	if (sh < 0) return sgn ^ exp ^ (i << -sh);
	// dosuwamy do lewej bity, które by wypadły
	// na dwa razy żeby uniknąć UB
	uint32_t grs = (i << 9) << (23 - sh);
	uint32_t sticky = grs << 2;
	sticky = (sticky | -sticky) >> 31; // sprawdzamy, czy sticky niezerowy
	grs = (grs >> 29) | sticky;
	uint32_t result = exp ^ (i >> sh);
	if (grs > 4) result++;
	if (grs == 4 && (result & 1)) result++;
	return sgn ^ result;
}

void test(int32_t i) {
	int32_t res = int2float(i);
	printf("%d %f\n", i, * (float*) &res);
}

float cast(int32_t i) {
	int32_t res = int2float(i);
	return * (float*) &res;
}

int main() {
	/*
	for (int i = -10; i < 17; i++) test(i);
	test(1048576);
	test(-1048576);
	test(INT_MIN);
	*/
	for (int i = INT_MIN; ; i++) {
		if ((float) i != cast(i)) printf("%d %f %f\n", i, (float) i, cast(i));
		if (i == INT_MAX) break;
	}
	return 0;
}
