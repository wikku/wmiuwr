CFLAGS=-Wall -Wextra -Werror=format -Werror=return-type -Werror=uninitialized -g
#CFLAGS += -fsanitize=undefined
CFLAGS += -O3

bin: 2.c Makefile
	$(CC) $(CFLAGS) -o bin 2.c

.PHONY: clean
clean:
	rm bin
