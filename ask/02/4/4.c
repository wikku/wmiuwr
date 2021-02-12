#include <stdio.h>
#include <stdint.h>
#define eprintf(...) fprintf(stderr, __VA_ARGS__)

uint32_t bytewise_add(uint32_t x, uint32_t y) {
	uint32_t s = (x & 0x7F7F7F7F) + (y & 0x7F7F7F7F);
	return ((x^y) & 0x80808080) ^ s;
}

uint32_t bytewise_sub(uint32_t x, uint32_t y) {
	uint32_t s = (x | 0x80808080) - (y & 0x7F7F7F7F);
	return ~((x^y | 0x7F7F7F7F) ^ s);
}

const uint32_t m = 0x00FF00FF;

// xoruje wynik dla bajtow parzystych z wynikiem dla bajtow nieparzystych

uint32_t bytewise_add2(uint32_t x, uint32_t y) {
	return (x & m) + (y & m) & m ^ (x & ~m) + (y & ~m) & ~m;
	// kiedy dodaje bajty parzyste, to zeruje sobie bajty nieparzyste i wtedy carry
	// z dodawania moze zmienic tylko bajty nieparzyste (ktore mnie teraz nie obchodza)
}

uint32_t bytewise_sub2(uint32_t x, uint32_t y) {
	return (x | m) - (y & ~m) & ~m ^ (x | ~m) - (y & m) & m;
	// kiedy odejmuje bajty parzyste, to wypelniam bajty nieparzyste jedynkami wiec
	// pozyczka wplynie tylko na nie (ktore mnie teraz nie obchodza)
}


#define test(x,y) do { printf("%d %d\n", bytewise_add(x,y), bytewise_add2(x,y)); } while (0)
#define testsub(x,y) do { printf("%d %d\n", bytewise_sub(x,y), bytewise_sub2(x,y)); } while (0)

int main() {
	test(0xabcdef98, 0xdeadcafe);
	test(0xFFFFFFFF, 0xdeadbeef);

	test(0x55555555, 0x22222222);
	// ~0x22 == 0xdd
	// 0xdd + 1 == 0xde
	testsub(0x55555555, 0xdededede);

	testsub(0xabcdef98, 0xdeadcafe);
	testsub(0xFFFFFFFF, 0xdeadbeef);
	testsub(0xFFFFFFFF, 0xFFFFFFFF);
	testsub(0x00000000, 0xFFFFFFFF);
	testsub(0x01020304, 0x1F2F3F4F);
	return 0;
}
