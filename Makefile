CFLAGS=-Wall -Werror -g

all:
	gcc $(CFLAGS) -I./include -o oleg.o -c ./src/oleg.c
	gcc $(CFLAGS) -I./include -o main.o -c ./src/main.c
	gcc $(CFLAGS) main.o oleg.o -o olegdb

clean:
	rm *.o
	rm olegdb
