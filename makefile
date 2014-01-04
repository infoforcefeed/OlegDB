CFLAGS=-Wall -Werror

all:
	gcc $(CFLAGS) -I./include -o oleg.o -c ./src/oleg.c
	gcc $(CFLAGS) -I./include -o main.o -c ./src/main.c
	gcc $(CFLAGS) main.o oleg.o -o olegdb
