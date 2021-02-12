CFLAGS=-Wall -Wextra -Werror=format -Werror=return-type -Werror=uninitialized -g
CFLAGS += -fsanitize=undefined
CFLAGS += -O2

bin: 2.c 2.s Makefile
	$(CC) $(CFLAGS) -o bin 2.c 2.s

.PHONY: clean
clean:
	rm bin
