CFLAGS=-Wall -Wextra -Werror=format -Werror=return-type -Werror=uninitialized -fsanitize=undefined -g -m32
CFLAGS+=-O2

bin: 3.c Makefile
	$(CC) $(CFLAGS) -o bin 3.c

.PHONY: clean
clean: 
