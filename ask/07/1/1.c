#include <stdio.h>
#include <stdint.h>
#define eprintf(...) fprintf(stderr, __VA_ARGS__)

#define B 5
#define A 9


typedef struct {
	int x[A][B];
	long y;
} str1;

typedef struct {
	char array[B];
	int t;
	short s[A];
	long u;
} str2;

void set_val(str1 *p, str2 *q) {
	long v1 = q->t;
	long v2 = q->u;
	p->y = v1 + v2;
}
/*
set_val:
	movslq 8(%rsi),%rax
	addq   32(%rsi),%rax
	movq   %rax,184(%rdi)
	ret
*/

/*
str2.t ma offset 8 i wymaga align 4, więc B jest w [5,8]
str2.s ma offset 12
str2.u ma offset 32 i wymaga align 8, więc A jest w [7,10]

str1.y ma offset 184=4*46 i wymaga align 8
więc A*B jest w [45,46]
jedyna możliwość B=5,A=9
*/

int main() {
	return 0;
}
