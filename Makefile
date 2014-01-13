CFLAGS=-Wall -Werror -g
uname_S := $(shell sh -c 'uname -s 2>/dev/null || echo not')
cc=gcc -std=c99

MATH_LINKER=
ifeq ($(uname_S),Darwin)
	# Do nothing
else
	MATH_LINKER=-lm
endif

all:
	$(cc) $(CFLAGS) -I./include -o oleg.o -c ./src/oleg.c
	$(cc) $(CFLAGS) -I./include -o test.o -c ./src/test.c
	$(cc) $(CFLAGS) -I./include -o server.o -c ./src/server.c
	$(cc) $(CFLAGS) -I./include -o main.o -c ./src/main.c
	$(cc) $(CFLAGS) main.o test.o server.o oleg.o $(MATH_LINKER) -o olegdb

clean:
	rm *.o
	rm olegdb
