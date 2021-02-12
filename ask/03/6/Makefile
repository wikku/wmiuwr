CFLAGS=-Wall -Wextra -Werror=format -Werror=return-type -Werror=uninitialized -g
CFLAGS += -fsanitize=undefined
CFLAGS += -O2

bin: 6.c Makefile
	$(CC) $(CFLAGS) -o bin 6.c

.PHONY: clean
clean:
	rm bin
