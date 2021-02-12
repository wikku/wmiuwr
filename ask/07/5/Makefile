CFLAGS=-Wall -Wextra -Werror=format -Werror=return-type -Werror=uninitialized -g
#CFLAGS += -fsanitize=undefined
CFLAGS += -Og
CFLAGS += -mpreferred-stack-boundary=2
CFLAGS += -m32

bin: 5.c Makefile
	$(CC) $(CFLAGS) -o bin 5.c

.PHONY: clean
clean:
	rm bin
