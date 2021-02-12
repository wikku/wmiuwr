#include <stdio.h>
#include <stdint.h>
#include <limits.h>
#include <assert.h>
#define eprintf(...) fprintf(stderr, __VA_ARGS__)

typedef union {
	float f;
	uint32_t u;
} U;

const uint32_t INF = 0x7F800000; // jednoczesnie maska wykladnika

uint32_t mulpow2(uint32_t x, int i) {
	// usuwamy znak, dodamy na koniec
	uint32_t sgn = x & 0x80000000;
	x ^= sgn;
	int32_t exp = x >> 23;
	if (i >= 0) {
		if (exp >= 255 - i) return sgn ^ INF;
		return sgn ^ (x & ~INF) ^ (exp + i) << 23;
	} else {
		// zmniejszamy wykladnik zeby liczba jeszcze byla znormalizowana
		int32_t tmp = i < 1-exp ? 1-exp : i;
		x = (x & ~INF) ^ (exp + tmp) << 23;
		i -= tmp;
		if (i < -23) return sgn; // unikamy UB przy zbyt duzym shifcie
		x >>= -i; // uwaga: przesuwamy jedynkę z wykladnika do mantysy
		return sgn ^ x;
	}
}


U _mp2test(U u, int i) {
	eprintf("%e\t%.8X * 2^%d => ", u.f, u.u, i);
	u.u = mulpow2(u.u, i);
	eprintf("\t%e\t%.8X\n", u.f, u.u);
	return u;
}

U testu(uint32_t f, int i) {
	U u;
	u.u = f;
	return _mp2test(u, i);
}

U testf(float f, int i) {
	U u;
	u.f = f;
	return _mp2test(u, i);
}

int main() {
	assert(mulpow2(0x7f000000, 0) == 0x7f000000);
	assert(mulpow2(0x00800000, 0) == 0x00800000);
	assert(mulpow2(0x7f7fffff, 0) == 0x7f7fffff);
	assert(mulpow2(0x00400000, 0) == 0x00400000);
	assert(mulpow2(0xff000000, 0) == 0xff000000);
	assert(mulpow2(0x80800000, 0) == 0x80800000);
	assert(mulpow2(0xff7fffff, 0) == 0xff7fffff);
	assert(mulpow2(0x80400000, 0) == 0x80400000);
	assert(testf(1.0f, 2).f == 4.0f);
	assert(testf(1.0f, 1).f == 2.0f);
	assert(testf(1.0f, 0).f == 1.0f);
	assert(testf(1.0f, -1).f == 0.5f);
	assert(testf(-1.0f, 2).f == -4.0f);
	assert(testf(-1.0f, 1).f == -2.0f);
	assert(testf(-1.0f, 0).f == -1.0f);
	assert(testf(-1.0f, -1).f == -0.5f);
	assert(testu(0x0000dead, -500).u == 0);
	assert(testu(0x0000dead, 500).u == INF);
	testu(0x7F000000, 1);
	testu(0x7F7FFFFF, 1);
	testu(0x7F000000, INT_MAX);
	testu(0x7F000000, -127);
	testu(0x7F7FFFFF, -127);
	testu(0x00800000, 1);
	testu(0x00800000, -1);
	testu(0x00ffffff, 1);
	testu(0x00ffffff, 0);
	testu(0x00ffffff, -1);
	testu(0x00ffffff, -2);
	testu(0x00ffffff, -3);
	testu(0x00ffffff, -4);
	testu(0x00ffffff, -5);
	testu(0x00ffffff, -21);
	testu(0x00ffffff, -22);
	testu(0x00ffffff, -23);
	testu(0x00ffffff, -24);
	return 0;
}
