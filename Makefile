CFLAGS=-Wall -Werror -g

all:
	gcc $(CFLAGS) -I./include -o oleg.o -c ./src/oleg.c
	gcc $(CFLAGS) -I./include -o test.o -c ./src/test.c
	gcc $(CFLAGS) -I./include -o server.o -c ./src/server.c
	gcc $(CFLAGS) -I./include -o main.o -c ./src/main.c
	gcc $(CFLAGS) main.o test.o server.o oleg.o -o olegdb

clean:
	rm *.o
	rm olegdb
