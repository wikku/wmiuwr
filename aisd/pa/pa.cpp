#include <cstdio>
#include <vector>
#include <cstdint>
#include <array>
#include <list>
#define eprintf(...) fprintf(stderr, __VA_ARGS__)

using namespace std;


//const auto P = 2137237;
const auto P = 422137;

array<vector<pair<uint32_t, uint64_t>>, P> arr;
void insert(uint32_t k, uint64_t v) {
	vector<pair<uint32_t, uint64_t>>& lst = arr[k % P];
	for (auto& p : lst) {
		if (p.first == k) {
			p.second += v;
			return;
		}
	}
	lst.push_back({k, v});
}

int main() {
	int m;
	scanf("%d", &m);
	for (int i = 0; i < m; i++) {
		uint32_t d, n;
		scanf("%u %u", &d, &n);
		uint32_t e = __builtin_ctz(d);
		uint32_t f = d >> e;
		uint64_t a = uint64_t(n) << e;
		//eprintf("d=%u n=%u e=%u f=%u a=%lu\n", d, n, e, f, a);
		insert(f, a);
	}
	uint64_t result = 0;
	for (auto &lst : arr) {
		for (auto &p : lst) {
			//eprintf("p.second=%lu", p.second);
			auto ones = __builtin_popcountll(p.second);
			result += ones;
			//eprintf("dbg: ones=%d\n", ones);
		}
	}
	printf("%lu\n", result);
	return 0;
}

