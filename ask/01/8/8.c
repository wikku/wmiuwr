#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <assert.h>
#define eprintf(...) fprintf(stderr, __VA_ARGS__)

void secret(uint8_t *to, uint8_t *from, size_t count) {
	assert(count > 0);
	size_t n = (count + 7) / 8;
	switch (count % 8) {
	case 0: do {	*to++ = *from++; // fall through
	case 7:			*to++ = *from++; // fall through
	case 6:			*to++ = *from++; // fall through
	case 5:			*to++ = *from++; // fall through
	case 4:			*to++ = *from++; // fall through
	case 3:			*to++ = *from++; // fall through
	case 2:			*to++ = *from++; // fall through
	case 1:			*to++ = *from++; // fall through
				} while (--n > 0);
	}
}

void secret2(uint8_t *to, uint8_t *from, size_t count) {
	assert(count > 0);
	size_t n = (count + 7) / 8;
	static void *array[] = { &&l0, &&l1, &&l2, &&l3, &&l4, &&l5, &&l6, &&l7 };
	goto *array[count % 8];
l0:	*to++ = *from++;
l7:	*to++ = *from++;
l6:	*to++ = *from++;
l5:	*to++ = *from++;
l4:	*to++ = *from++;
l3:	*to++ = *from++;
l2:	*to++ = *from++;
l1:	*to++ = *from++;
	if (--n > 0) goto l0;
}

typedef void (*copy_fn)(uint8_t*, uint8_t*, size_t);

void test(size_t n, copy_fn f) {
	char *source = malloc(n+1);
	source[n] = 0;
	for (size_t i = 0; i < n; i++) {
		source[i] = 'a' + (i % 26);
	}
	char *target = malloc(n+1);
	(*f)((void*) target, (void*) source, n);
	printf("target %s\n", target);
	free(source);
	free(target);
}

int main() {
	test(12, &secret);
	test(12, &secret2);
	test(8, &secret);
	test(8, &secret2);
	test(1, &secret);
	test(1, &secret2);
	test(32, &secret);
	test(32, &secret2);
	return 0;
}
