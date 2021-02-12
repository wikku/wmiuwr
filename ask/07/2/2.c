#include <stdio.h>
#include <stdint.h>
#define eprintf(...) fprintf(stderr, __VA_ARGS__)

#define R 7
#define S 5
#define T 13

long A[R][S][T];

long store_elem(long i /* rdi */, long j /* rsi */,
                long k /* rdx */, long *dest /* rcx */) {
	*dest = A[i][j][k];
	return sizeof(A);
}

/*
3640 = 8 * R * S * T
 455 = R * S * T

factor 455
455: 5 7 13

wiemy, że (R,S,T) to permutacja (5,7,13)

*/

/*
store_elem:
	leaq (%rsi,%rsi,2),%rax ; %rax <- 3*%rsi
	leaq (%rsi,%rax,4),%rax ; %rax <- 4*%rax + %rsi
	; w %rax mamy 13*j
	movq %rdi,%rsi
	salq $6,%rsi
	; w %rsi mamy 64*i
	addq %rsi,%rdi
	addq %rax,%rdi
	; w %rdi mamy 65*i + 13*j = 13*(5*i+j)
	; wnioskujemy, że R = 7, S = 5, T = 13
*/

int main() {
	return 0;
}
