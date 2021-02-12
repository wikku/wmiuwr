#include <stdio.h>
#include <stdint.h>
#define eprintf(...) fprintf(stderr, __VA_ARGS__)

uint32_t ce(uint32_t x) {
	uint32_t dst;
	asm (
		"mov %1, %%eax	\t\n"
		"rol $8,%%ax		\t\n"
		"rol $16,%%eax	\t\n"
		"rol $8,%%ax		\t\n"
		"mov %%eax,%0	\t\n"
		: "=r" (dst)
		: "r" (x)
		: "%eax");
	return dst;
}

uint32_t ror16(uint32_t x) {
	return x << 16 | x >> 16;
}

void testce(uint32_t x) {
	printf("%x %x\n", x, ce(x));
}

int main() {
	testce(0xABCDEF21);
	testce(0x19283456);
	return 0;
}
