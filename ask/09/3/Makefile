CFLAGS=-Wall -Wextra -Werror=format -Werror=return-type -Werror=uninitialized -g
#CFLAGS += -fsanitize=undefined
CFLAGS += -Og

bin: 3.c Makefile
	$(CC) $(CFLAGS) -o bin 3.c

.PHONY: clean
clean:
	rm bin
