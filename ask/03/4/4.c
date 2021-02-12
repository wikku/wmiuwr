#include <stdio.h>
#include <stdint.h>
#include <limits.h>
#define eprintf(...) fprintf(stderr, __VA_ARGS__)

// 1. (float) x == (float) dx to zawsze prawda, bo double dokładnie
// reprezentuje cały zakres int32_t i zaokrąglanie przy rzutowaniu działa
// tak samo
//
// 2. dx - dy == (double)(x-y) nie musi być prawdą z powodu overflowów
// np. -2147483648.0 - 1.0 != (double) ((int32_t) -2147483648-1);
//
// 3. (dx + dy) + dz == dx + (dy + dz) to zawsze prawda, bo double potrafi
// reprezentować wszystkie liczby całkowite do 2^53 i nie trzeba zaokrąglać
//
// 4. (dx * dy) * dz == dx * (dy * dz) nie musi być prawdą z powodu utraty precyzji
// np. x = 2147483587 y = 2147483587 z = 5
//
// 5. dx / dx == dz / dz to fałsz dla dx = 0


int main() {
	double p1 = 2147483587.0, p2 = 2147483693.0;
	for (double x = p1; x < INT_MAX; x += 1.0) {
		for (double y = p1; y < INT_MAX; y += 1.0) {
			for (double z = 0.0; z < 256; z += 1.0) {
				if ((x*y)*z != x*(y*z)) printf("%f %f %f %f %f\n", x, y, z, (x*y)*z, x*(y*z));
			}
		}
	}

	printf("%f %f\n", -2147483648.0 - 1.0, (double) ((int32_t) -2147483648-1));
	for (int x = INT_MIN; ; x++) {
		int y = INT_MAX;
		double dy = (double) y;
		double dx = (double) x;
		//if (dx - dy != (double)(x - y)) printf("%d %d\n", x, y);

		if (x == INT_MAX) break;
	}

	return 0;
}
