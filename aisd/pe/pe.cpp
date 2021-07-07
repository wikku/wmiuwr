#include <cstdio>
#include <set>
#include <vector>
#include <unordered_map>
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
	vector<point> points(2*n);
	vector<line_type> lt(n);
	vector<pii> result;
	for (int i = 0; i < n; i++) {
		int x1, y1, x2, y2;
		scanf("%d %d %d %d", &x1, &y1, &x2, &y2);
		points[2*i].x = 2*x1;
		points[2*i].y = 2*y1;
		points[2*i+1].x = 2*x2;
		points[2*i+1].y = 2*y2;
		points[2*i].id = points[2*i+1].id = 2*i;
		if (x1 == x2) { lt[i] = VERT; }
		else if (y1 == y2) { lt[i] = HORI; }
		else if ((x1 - x2 > 0) == (y1 - y2 > 0)) { lt[i] = DIAG; }
		else if ((x1 - x2 > 0) != (y1 - y2 > 0)) { lt[i] = ADIA; }
		else { eprintf("line_type bug"); }
		//eprintf("lt=%d\n", int(lt[i]));
	}
	vector<xy> start(n), end(n);
	set<pii> state[LINE_TYPES];
	for (auto sweep_orientation : line_types) {
		//eprintf("SWEEP %d\n", int(sweep_orientation));
		//for (auto i : end_count) { eprintf("%llx:%d ", i.first, i.second); } puts("");
		vector<point> pts = points;
		auto compare = cmp(sweep_orientation);
		for (int i = 0; i < n; i++) {
			pts[2*i+compare(pts[2*i], pts[2*i+1])].id++;
		}
		sort(pts.begin(), pts.end(), compare);
		int prevx = -1;
		int prevy = -1;
		for (auto p : pts) {
			if (sweep_orientation == VERT && prevx == p.x && prevy == p.y) {
				result.push_back({ p.x, p.y });
			}
			prevx = p.x; prevy = p.y;
			//eprintf("%d %d %d\n", p.x, p.y, p.id);
			const xy xy = { p.x, p.y };
			const int lid = p.id >> 1;
			const bool exit = p.id & 1;
			const line_type t = lt[lid];
			if (!exit) {
				start[lid] = xy;
				state[t].insert({ id(t, xy), lid });
			} else {
				state[t].erase({ id(t, xy), lid });
				if (t != sweep_orientation) continue;
				for (auto t2 : line_types) {
					if (t2 == t) continue;
					pii rng = idrange(t2, xy, start[lid]);
					/*
					eprintf("rng={ %d, %d }\n", rng.first, rng.second);
					eprintf("state[%d] ", int(t2));
					for (auto p : state[t2]) { eprintf("%d %d  ", p.first, p.second); }
					eprintf("\n");
					*/
					auto it = state[t2].lower_bound({ rng.first, -9999999 });
					while (it != state[t2].end() && it->first <= rng.second) {
						auto i = intersection(t, id(t, xy), t2, it->first);
						//eprintf("lol3\n");
						result.push_back(i);
						it++;
					}
				}
			}
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
