#include <stdio.h>
#include <stdint.h>
#include <limits.h>
#define eprintf(...) fprintf(stderr, __VA_ARGS__)

int32_t odd_ones(uint32_t x) {
	// chcemy zXORowac wszystkie bity x
    x = x ^ x >> 1; // teraz bity parzyste przechowują XOR par bitów
    x = x ^ x >> 2; // teraz co czwarty bit przechowuje XOR czwórki bitów
    x = x ^ x >> 4; // itd.
    x = x ^ x >> 8;
    x = x ^ x >> 16;
    // zwracamy bit 0, który przechowuje XOR wszystkich bitów oryginalnego x
    return x & 1;
}

int main() {
	for (int i = INT_MIN; ; i++) {
		if (odd_ones(i) != (__builtin_popcount(i) & 1))
			printf("%d\n", i);
		if (i == INT_MAX) break;
	}
	return 0;
}
