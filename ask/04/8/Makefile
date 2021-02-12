CFLAGS=-Wall -Wextra -Werror=format -Werror=return-type -Werror=uninitialized -g
#CFLAGS += -fsanitize=undefined
CFLAGS += -O

bin: 8.c Makefile
	$(CC) $(CFLAGS) -o bin 8.c

.PHONY: clean
clean:
	rm bin
