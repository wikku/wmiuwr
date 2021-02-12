#include <stdio.h>
#include <stdint.h>
#define eprintf(...) fprintf(stderr, __VA_ARGS__)

#define CNT 7

typedef struct {
	int idx;
	long x[4];
} a_struct;

typedef struct {
	int first;
	a_struct a[CNT];
	int last;
} b_struct;

void test(long i /* %rdi */, b_struct *bp /* %rsi */) {
	int n = bp->first + bp->last;
	a_struct *ap = &bp->a[i];
	ap->x[ap->idx] = n;
}


/*
test:
	movl   0x120(%rsi),%ecx ; b_struct.last ma offset 0x120=288
	addl   (%rsi),%ecx
	; w %ecx mamy n
	leaq   (%rdi,%rdi,4),%rax ; %rax <- 5*%rdi
	leaq   (%rsi,%rax,8),%rax ; %rax <- 8*%rax + %rsi
	; wnioskujemy, że sizeof(a_struct) = 5*8 = 40
	; w %rax mamy (void*) bp + 40*i
	movq   0x8(%rax),%rdx ; b_struct.a ma offset 8
	; w %rdx mamy bp->a[i].idx
	movslq %ecx,%rcx
	movq   %rcx,0x10(%rax,%rdx,8)
	; 0x10 to offset b_struct.a + offset a_struct.x
	retq
*/


int main() {
	printf("%ld %ld\n", sizeof(a_struct), sizeof(b_struct));
	return 0;
}
