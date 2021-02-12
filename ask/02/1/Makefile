CFLAGS=-Wall -Wextra -Werror=format -Werror=return-type -Werror=uninitialized -g
CFLAGS += -fsanitize=undefined
CFLAGS += -O2
CFLAGS += -fwrapv

bin: 1.c Makefile
	$(CC) $(CFLAGS) -o bin 1.c

.PHONY: clean
clean:
	rm bin
