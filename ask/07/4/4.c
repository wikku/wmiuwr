#include <stdio.h>
#include <stdint.h>
#define eprintf(...) fprintf(stderr, __VA_ARGS__)

union elem {
	struct {
		long *p;
		long y;
	} e1;
	struct {
		long x;
		union elem *next;
	} e2;
};

/* oba warianty mają rozmiar 16 bajtów, więc elem także */

union elem *proc(union elem *p /* %rdi */) {
	union elem *q /* %rax */ = p->e2.next;
	p->e2.x = *q->e1.p - q->e1.y;
	return q;
}

int main() {
	printf("%ld\n", sizeof(union elem));
	return 0;
}
