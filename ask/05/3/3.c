#include <stdio.h>
#include <stdint.h>
#include <limits.h>

uint32_t puzzle3(uint32_t n, uint32_t d);

/* dzielenie n przez d */
uint32_t div(uint32_t n /* %edi */, uint32_t d /* %esi */) {
	uint64_t n64 /* %rdi */ = n;
	uint64_t d64 /* %rsi */ = (uint64_t) d << 32;
	uint32_t result /* %eax */ = 0;
	for (uint32_t mask /* %ecx */ = 0x80000000; mask; mask >>= 1) {
		/* w assembly był jeszcze redundantny %edx,
		   który mówił, który bit mask jest zapalony (indeksując od 1) */
		n64 <<= 1;
		uint64_t tmp /* %r8 */ = n64 - d64;
		if (~tmp >> 63) {
			result |= mask;
			n64 = tmp;
		}
	}
	return result;
}


int main() {
	printf("x\t"); for (uint32_t j = 0; j < 16; j++) printf("%u\t", j); puts("");
	for (uint32_t i = 0; i < 16; i++) {
		printf("%.2u\t", i);
		for (uint32_t j = 0; j < 16; j++) {
			printf("%d\t", puzzle3(i, j));
		}
		puts("");
		printf("%.2u\t", i);
		for (uint32_t j = 0; j < 16; j++) {
			printf("%d\t", div(i, j));
		}
		puts("");
	}
	printf("%d %d", puzzle3(UINT_MAX, INT_MAX), div(UINT_MAX, INT_MAX));
	return 0;
}
