CFLAGS=-Wall -Werror -g
uname_S := $(shell sh -c 'uname -s 2>/dev/null || echo not')

MATH_LINKER=
ifeq ($(uname_S),Darwin)
	# Do nothing
else
	MATH_LINKER=-lm
endif

all:
	gcc $(CFLAGS) -I./include -o murmur3.o -c ./src/murmur3.c
	gcc $(CFLAGS) -I./include -o oleg.o -c ./src/oleg.c
	gcc $(CFLAGS) -I./include -o test.o -c ./src/test.c
	gcc $(CFLAGS) -I./include -o parsing.o -c ./src/parsing.c
	gcc $(CFLAGS) -I./include -o server.o -c ./src/server.c
	gcc $(CFLAGS) -I./include -o main.o -c ./src/main.c
	gcc $(CFLAGS) main.o test.o parsing.o server.o oleg.o murmur3.o $(MATH_LINKER) -o olegdb

clean:
	rm *.o
	rm olegdb
