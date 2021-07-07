#include <cstdio>
#include <algorithm>
#include <bitset>
#include <vector>
#define eprintf(...) fprintf(stderr, __VA_ARGS__)

using namespace std;

typedef long long ll;
typedef unsigned long long ull;

const int M = 1048576;

// hbd[i][d] — największa wysokość wieży większej o d od drugiej zbudowanych z pierwszych i klocków
// add_block(hbd, h)[d] =
//   max(hbd[d],               nie dodajemy
//       hbd[d+h],             dodajemy do mniejszej wieży, ale nie przewyższając drugiej
//       if d < h
//         then hbd[h-d]+d    dodajemy do niższej wieży
//         else hbd[d-h]+h    dodajemy do wyższej wieży
//      )
// = max(hbd[d], hbd[d+h], hbd[|d-h|] + min(d,h))

int cur[M];
int tmp[M];

int main() {
	int n;
	scanf("%d", &n);
	vector<int> hs(n);
	for (int i = 0; i < n; i++) {
		scanf("%d", &hs[i]);
	}
	sort(hs.begin(), hs.end());
	fill(cur, cur+M, -99999999);
	fill(tmp, tmp+M, -99999999);
	int sum = 0;
	//int dbg_copies = 0;
	for (int i = 0; i < n; i++) {
		int h = hs[i];
		// powiększ dodając do niższej
		for (int d = 0; d < h; d++) {
			tmp[d] = max(tmp[d], cur[h-d]+d);
		}
		// powiększ dodając do wyższej
		for (int d = h, m=min(sum+h, M-1); d <= m; d++) {
			tmp[d] = max(tmp[d], cur[d-h]+h);
		}
		// dodaj do niższej nie powiększając
		for (int d = 0, m = sum-h; d <= m; d++) {
			tmp[d] = max(tmp[d], cur[d+h]);
		}
		tmp[h] = max(tmp[h], h);
		sum += h;
		/*int dbg_diffs = 0;
		for (int i = 0; i < nmaxi+1; i++) {
			if (cur[i] != tmp[i]) dbg_diffs++;
		}
		eprintf("i=%d diffs=%d/%d\n", i, dbg_diffs, nmaxi+1);
		*/
		copy(tmp, tmp+sum+1, cur);
		//dbg_copies += nmaxi+1;
		//for (int d = 0; d < 10; d++) eprintf("%d ", cur[d]); eprintf("\n");
	}
	//eprintf("copies=%d\n", dbg_copies);
	if (cur[0] > 0) {
		puts("TAK");
		printf("%d\n", cur[0]);
	} else {
		puts("NIE");
		for (int d = 1; d < M; d++) {
			if (cur[d] > d) {
				printf("%d\n", d);
				break;
			}
		}
	}
	return 0;
}

