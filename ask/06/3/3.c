#include <stdio.h>
#include <stdint.h>
#define eprintf(...) fprintf(stderr, __VA_ARGS__)

/* być może zwraca wynik typu niewiększego niż 128 bitów,
 * ale w puzzle5 nie korzystamy z wyniku */
void readlong(long *x) {
	scanf("%ld", x);
};

/* wczytuje dwa longi i sprawdza, czy drugi dzieli pierwszy */
long puzzle5(void) {
	/* stack frame ma 32 bajty i składa się z
	 * adresu powrotu, paddingu i zmiennych x i y */
	long x, y;
	readlong(&x);
	readlong(&y);
	return x % y == 0;
}

int main() {
	printf("%ld\n", puzzle5());
	return 0;
}
