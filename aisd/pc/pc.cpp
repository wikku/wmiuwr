#include <cstdio>
#include <numeric>
#include <array>
#include <vector>
#define eprintf(...) fprintf(stderr, __VA_ARGS__)

using namespace std;

typedef long long ll;

int cbm[1024];
int tmp[1024];
unsigned compatible[1024][32];
unsigned indices[1024][32];

void debug_mask10(unsigned m) {
	eprintf("%d%d\n", m>>0&1, m>>5&1);
	eprintf("%d%d\n", m>>1&1, m>>6&1);
	eprintf("%d%d\n", m>>2&1, m>>7&1);
	eprintf("%d%d\n", m>>3&1, m>>8&1);
	eprintf("%d%d\n", m>>4&1, m>>9&1);
}
void debug_mask15(array<array<bool, 3>, 5> c) {
	for (int i = 0; i < 5; i++) {
		for (int j = 0; j < 3; j++) {
			eprintf("%d", c[i][j]);
		}
		eprintf("\n");
	}
}

void mark_incompatible(array<array<bool, 3>, 5> c) {
	//debug_mask15(c);
	int m1 = 0;
	for (int i = 1; i >= 0; i--) {
		for (int j = 4; j >= 0; j--) {
			m1 <<= 1;
			m1 |= c[j][i];
		}
	}
	int m2 = 0;
	for (int i = 2; i >= 1; i--) {
		for (int j = 4; j >= 0; j--) {
			m2 <<= 1;
			m2 |= c[j][i];
		}
	}
	//debug_mask10(m1);
	//debug_mask10(m2);
	m1 %= 32;
	//eprintf("compatible[%d][%d] = 0\n", m2, m1);
	compatible[m2][m1] = 0;
}

array<array<bool, 3>, 8> rows = { {
	{ 0, 0, 0 },
	{ 0, 0, 1 },
	{ 0, 1, 0 },
	{ 0, 1, 1 },
	{ 1, 0, 0 },
	{ 1, 0, 1 },
	{ 1, 1, 0 },
	{ 1, 1, 1 }
} };

void update(array<array<bool, 3>, 3> p) {
	for (int r1 = 0; r1 < 8; r1++) {
		for (int r2 = 0; r2 < 8; r2++) {
			mark_incompatible({rows[r1], rows[r2], p[0], p[1], p[2]});
			mark_incompatible({rows[r1], p[0], p[1], p[2], rows[r2]});
			mark_incompatible({p[0], p[1], p[2], rows[r1], rows[r2]});
		}
	}
}

int main() {
	int n, p, mod;
	scanf("%d %d %d", &n, &p, &mod);
	fill(&compatible[0][0], &compatible[0][0]+1024*32, 0xFFFFFFFFu);
	fill(&indices[0][0], &indices[0][0]+1024*32, -1);
	for (int i = 0; i < p; i++) {
		array<array<bool, 3>, 3> p;
		for (int r = 0; r < 3; r++) {
			char line[5];
			scanf(" %s ", line);
			//eprintf("%s\n", line);
			for (int j = 0; j < 3; j++)
				p[r][j] = (line[j] == 'x');
		}
		update(p);
	}
	for (int m = 0; m < 1024; m++) {
		int end = 0;
		for (int pm0 = (m*32) % 1024, pm = 0; pm < 32; pm++) {
			if (compatible[m][pm]) {
				indices[m][end++] = pm0+pm;
			}
		}
	}
	//for(int i = 0; i < 32; i++) eprintf("%d", compatible[0][i]&1); eprintf("");
	fill(cbm, cbm+1024, 1);
	for (int c = 3; c <= n; c++) {
		for (int m = 0; m < 1024; m++) {
			int res = 0;
			int j;
			for (int i = 0; i < 32; i++) {
				if ((j = indices[m][i]) < 0) break;
				res += cbm[j];
			}
			/*
			while ((j = indices[m][i]) != -1) {
				res += cbm[j];
				if (++i >= 32) break;
			}
			*/
			tmp[m] = res % mod;
		}
		copy(tmp, tmp+1024, cbm);
	}
	ll result = accumulate(cbm, cbm+1024, 0ll);
	result = ((result % mod) + mod) % mod;
	printf("%lld\n", result);
	return 0;
}

