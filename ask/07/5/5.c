#include <stdio.h>
#include <stdint.h>
#define eprintf(...) fprintf(stderr, __VA_ARGS__)

typedef struct A {
	long u[2];
	long *v;
} SA;

typedef struct B {
	long p[2];
	long q;
} SB;

SB eval(SA s) {
	return (SB) {
		{ *s.v * s.u[1], s.u[0] - *s.v },
		s.u[0] - s.u[1]
	};
}

long wrap(long x, long y, long z) {
	SA arg = { { x, y }, &z };
	SB sb = eval(arg);
	return (sb.p[0] + sb.p[1]) * sb.q;
}
/*

0   return address

-8

-16

-24

-32

-40

-48          \
             |
-56          | miejsce na sb
             |
-64          /

-72 z  <-\
         |
-80 &z --/   \
             |
-88 y        | arg
             |
-96 x        /
*/

int main() {
	//printf("%ld %ld\n", __alignof__(SA), __alignof__(SB));
	printf("%d %d\n", __alignof__(SA), __alignof__(SB));
	return 0;
}
