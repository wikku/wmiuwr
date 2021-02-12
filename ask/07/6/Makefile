CFLAGS=-Wall -Wextra -Werror=format -Werror=return-type -Werror=uninitialized -g
#CFLAGS += -fsanitize=undefined
#CFLAGS += -mavx512f
CFLAGS += -mfma
CFLAGS += -O2

bin: 6.c Makefile puzzle6.s
	$(CC) $(CFLAGS) puzzle6.s 6.c -o bin

.PHONY: clean
clean:
	rm bin
