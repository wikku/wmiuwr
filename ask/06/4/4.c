#include <stdio.h>
#include <stdint.h>
#include <stdarg.h>
#define eprintf(...) fprintf(stderr, __VA_ARGS__)

/*
typedef struct {
	unsigned int gp_offset;
	unsigned int fp_offset;
	void *overflow_arg_area;
	void *reg_save_area; // tu zapisujemy argumenty podane w rejestrach
} va_list[1];
*/

long puzzle7(long n, ...) {
	va_list args;
	va_start(args, n);
	long result = 0;
	for ( ; n > 0; n--) {
		result += va_arg(args, long);
	}
	va_end(args);
	return result;
}


int main() {
	printf("%ld\n", puzzle7(2, 3, 4));
	return 0;
}
