#include <cstdio>
#include <set>
#include <vector>
#include <unordered_set>
#include <map>
#include <algorithm>
#include <functional>
#define eprintf(...) fprintf(stderr, __VA_ARGS__)

using namespace std;

typedef long long ll;
typedef unsigned long long ull;
typedef pair<int, int> pii;

struct point { int x; int y; int id; };
struct xy { int x; int y; };
xy toxy(point p) { return { p.x, p.y }; }
enum line_state { PRE, IN, POST };
enum line_type { VERT, DIAG, HORI, ADIA };
const int LINE_TYPES = 4;
const line_type line_types[LINE_TYPES] = { VERT, DIAG, HORI, ADIA };

const int xc[LINE_TYPES] = { 1,  1,  0,  1 };
const int yc[LINE_TYPES] = { 0, -1,  1,  1 };
int det(int a, int b, int c, int d) { return a*d - b*c; }
int id(line_type lt, xy p) { return xc[lt] * p.x + yc[lt] * p.y; }

bool lex3cmp(tuple<int, int, int> x, tuple<int, int, int> y) { return x < y; }
function<bool(point, point)> cmp(line_type lt) {
	line_type lt2 = line_type ((lt + 2) % 4);
	return [lt, lt2](point a, point b){
		bool result = lex3cmp(
			{ id(lt, toxy(a)), id(lt2, toxy(a)), a.id },
			{ id(lt, toxy(b)), id(lt2, toxy(b)), b.id });
		return result;
	};
}


pii intersection(line_type lt1, int id1, line_type lt2, int id2) {
	int d = det(xc[lt1], yc[lt1], xc[lt2], yc[lt2]);
	int dx = det(id1, yc[lt1], id2, yc[lt2]);
	int dy = det(xc[lt1], id1, xc[lt2], id2);
	return { dx/d, dy/d };
}

pii idrange(line_type lt, xy p1, xy p2) {
	int id1 = id(lt, p1), id2 = id(lt, p2);
	if (id1 > id2) swap(id1, id2);
	return { id1, id2 };
}

void print_coord(int c) {
	printf("%d.", c/2);
	putc("05"[c&1], stdout);
	putc(' ', stdout);
}

ull mapkey(xy p) { return (ull(p.x) << 32) | p.y; }

int main() {
	int n;
	scanf("%d", &n);
	vector<pii> result;
	unordered_set<ull> points;
	for (int i = 0; i < n; i++) {
		int x1, y1, x2, y2;
		scanf("%d %d %d %d", &x1, &y1, &x2, &y2);
		x1 *= 2; y1 *= 2; x2 *= 2; y2 *= 2;
		int dist = max(abs(x1 - x2), abs(y1 - y2));
		int dx = (x2 - x1) / dist;
		int dy = (y2 - y1) / dist;
		int x = x1, y = y1;
		for (int j = 0; j <= dist; j++) {
			if (points.find(mapkey({ x, y })) != points.end()) {
				result.push_back({ x, y });
			} else {
				points.insert(mapkey({ x, y }));
			}
			x += dx;
			y += dy;
		}
	}
	sort(result.begin(), result.end());
	result.erase(unique(result.begin(), result.end()), result.end());
	if (result.empty()) {
		puts("BRAK");
		return 0;
	}
	for (auto xy : result) {
		print_coord(xy.first);
		print_coord(xy.second);
		puts("");
	}
	return 0;
}
