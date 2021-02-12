#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#define eprintf(...) fprintf(stderr, __VA_ARGS__)

struct P {
	uint64_t n;
	float arr[]; // https://en.wikipedia.org/wiki/Flexible_array_member
};

float puzzle6(struct P *, float);

/* oblicza wartość wielomianu w x */
float puzzle(struct P *p /* %rdi */, float x /* %xmm0 */) {
	float result /* %xmm1 */ = 0.0;
	float pow /* %xmm2 */ = 1.0;
	for (uint64_t i = 0; i < p->n; i++) {
		result += pow * p->arr[i];
		pow *= x;
	}
	return result;
}


int main() {
	const size_t N = 8;
	const size_t bytes = sizeof(struct P) + N * sizeof(float);
	struct P *ptr = malloc(bytes);
	ptr->n = N;
	for (size_t i = 0; i < N; i++)
		ptr->arr[i] = 1.0 + i;

	for (size_t i = 0; i <= N; i++) {
		ptr->n = i;
		printf("%f %f\n", puzzle(ptr, 1.0), puzzle6(ptr, 1.0));
		printf("%f %f\n", puzzle(ptr, 2.0), puzzle6(ptr, 2.0));
		printf("%f %f\n", puzzle(ptr, 3.0), puzzle6(ptr, 3.0));
		printf("%f %f\n", puzzle(ptr, 4.0), puzzle6(ptr, 4.0));
	}
	return 0;
}
