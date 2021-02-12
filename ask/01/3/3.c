#include <stdio.h>
#include <stdint.h>
#include <stddef.h>
#define eprintf(...) fprintf(stderr, __VA_ARGS__)

typedef struct A {
	int8_t a; // 1 byte
	// 3 byte padding
	void *b; // 4 bytes
	int8_t c; // 1 byte
	// 1 byte padding
	int16_t d; // 2 bytes
} A;

typedef struct A2 {
	int8_t a; // 1 byte
	int8_t c; // 1 byte
	int16_t d; // 2 bytes
	void *b; // 4 bytes
} A2;

typedef struct B {
	uint16_t a; // 2 bytes
	// 2 byte padding
	double b; // 8 bytes
	void *c; // 4 bytes
} B;

typedef struct B2 {
	double b; // 8 bytes
	void *c; // 4 bytes
	uint16_t a; // 2 bytes
} B2;

int main() {
	printf("sizeof(A) = %d\n", sizeof(A));
	printf("A offsets: a %d, b %d, c %d, d %d\n",
			offsetof(A, a),
			offsetof(A, b),
			offsetof(A, c),
			offsetof(A, d));

	printf("sizeof(A2) = %d\n", sizeof(A2));
	printf("A2 offsets: a %d, b %d, c %d, d %d\n",
			offsetof(A2, a),
			offsetof(A2, b),
			offsetof(A2, c),
			offsetof(A2, d));

	printf("sizeof(B) = %d\n", sizeof(B));
	printf("B offsets: a %d, b %d, c %d\n",
			offsetof(B, a),
			offsetof(B, b),
			offsetof(B, c));

	printf("sizeof(B2) = %d\n", sizeof(B2));
	printf("B2 offsets: a %d, b %d, c %d\n",
			offsetof(B2, a),
			offsetof(B2, b),
			offsetof(B2, c));

	return 0;
}
