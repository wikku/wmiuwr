#include <cstdio>
#include <vector>
#include <unordered_map>
#include <algorithm>
#include <queue>
#include <map>
#define eprintf(...) fprintf(stderr, __VA_ARGS__)

using namespace std;

typedef long long ll;
typedef unsigned long long ull;
typedef pair<int, int> pii;
typedef pair<int, pii> piii;

vector<int> dsu;

int find(int v) {
	if (dsu[v] == v) {
		return v;
	} else {
		return dsu[v] = find(dsu[v]);
	}
}

void unite(int a, int b) {
	dsu[find(a)] = find(b);
};


int main() {
	int n, m;
	scanf("%d %d", &n, &m);
	dsu.resize(n);
	for (int i = 0; i < n; i++) dsu[i] = i;
	vector<piii> edges;
	map<pii, int> max_weight;
	for (int i = 0; i < m; i++) {
		int a, b, w;
		scanf("%d %d %d", &a, &b, &w);
		a--; b--;
		if (b > a) swap(a, b);
		edges.push_back({ w, { a, b } });
	}
	sort(edges.begin(), edges.end());
	int critical = 2000000000;
	for (int i = m-1; i >= 0; i--) {
		int w = edges[i].first;
		int a = edges[i].second.first;
		int b = edges[i].second.second;
		if (find(a) != find(b)) {
			unite(a, b);
			critical = min(critical, w);
		}
	}
	printf("%d\n", critical);
	return 0;
}

