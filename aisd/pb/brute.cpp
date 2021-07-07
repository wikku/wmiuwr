#include <cstdio>
#include <bitset>
#include <vector>
#define eprintf(...) fprintf(stderr, __VA_ARGS__)

using namespace std;

typedef long long ll;
typedef unsigned long long ull;

const int M = 100;

bitset<M> add_to_smaller(bitset<M> b, int h) {
	bitset<M> r;
	for (int i = 0; i < h; i++) {
		if (bool(b[h-i]) == true) r[i] = 1;
	}
	return r | (b >> h);
}
bitset<M> add_to_bigger(bitset<M> b, int h) {
	return (b << h);
}

// diffs[1][s]
// diffs[i][s] — bitset możliwych różnic wysokości między wyższą a niższą wieżą zbudowaną z pierwszych i klocków
// diffs[i][s] = diffs[i-1][s]
//             | add_to_smaller(diffs[i-1], s-h[i])
//             | add_to_bigger(diffs[i-1][s-h[i]], h[i])



int main() {
	int n;
	scanf("%d", &n);
	vector<int> h(n);
	for (int i = 0; i < n; i++) {
		scanf("%d", &h[i]);
	}
	vector<bitset<M>> cur(M);
	for (int i = 0; i < n; i++) {
		//eprintf("i=%d\n", i);
		for (int s = M-1; s >= 0; s--) {
			if (s < h[i]) continue;
			//for (int b = 0; b < 60; b++) eprintf("%d", bool(cur[s-h[i]][b])); eprintf(" i=%d s=%d\n",i,s-h[i]);
			cur[s] |= add_to_smaller(cur[s-h[i]], h[i]) | add_to_bigger(cur[s-h[i]], h[i]);
			//for (int b = 0; b < 60; b++) eprintf("%d", bool(cur[s][b])); eprintf(" i=%d s=%d\n",i,s);
		}
		cur[h[i]][h[i]] = 1;
	}
	for (int s = M-1; s > 0; s--) {
		if (cur[s][0] == 1) {
			puts("TAK");
			printf("%d\n", s/2);
			//eprintf("s=%d\n", s);
			return 0;
		}
	}
	int m = M;
	for (int s = M-1; s > 0; s--) {
		for (int i = 1; i < s; i++) {
			if (cur[s][i] == 1) {
				m = min(m, i);
				break;
			}
		}
	}
	puts("NIE");
	printf("%d\n", m);
	return 0;
}

