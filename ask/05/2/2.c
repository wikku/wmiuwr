#include <stdio.h>
#include <stdint.h>
#define eprintf(...) fprintf(stderr, __VA_ARGS__)

/* na którym indeksie s jest znak, który nie jest w d */
long puzzle(char *s /* %rdi */, char *d /* %rsi */) {
	for (char* sp /* %rax */ = s; ; sp++) {
		char* dp /* %rdx */ = d;
		char cl;
		do {
			cl = *dp++;
			if (cl == '\0') {
				return sp - s;
			}
		} while (cl != *sp);
	}
}

long puzzle2(char *s, char *d);

int main(int argc, char** argv) {
	printf("%ld %ld\n", puzzle(argv[1], argv[2]), puzzle2(argv[1], argv[2]));
	return 0;
}
