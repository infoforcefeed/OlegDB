CFLAGS=-Wall -Werror -g3
uname_S := $(shell sh -c 'uname -s 2>/dev/null || echo not')
cc=gcc -std=c99
VERSION=0.1
BUILD_DIR=$(shell pwd)/build/
LIB_DIR=$(BUILD_DIR)lib/
BIN_DIR=$(BUILD_DIR)bin/
SONAME=liboleg.so.1
ERLFLAGS=-smp -W1 -Werror -b beam -I./include -o $(BIN_DIR)
ERLINCLUDES = $(shell echo 'io:format("~s~n",[code:root_dir()]),init:stop().' | erl | sed -n '/^1>/s/^1> //p')/usr/include/
INCLUDES=-I./include -I$(ERLINCLUDES)

MATH_LINKER=
ifeq ($(uname_S),Darwin)
	# Do nothing
else
	MATH_LINKER=-lm
endif

all: liboleg erlang

liboleg:
	$(cc) $(CFLAGS) $(INCLUDES) -c -fPIC ./src/murmur3.c
	$(cc) $(CFLAGS) $(INCLUDES) -c -fPIC ./src/oleg.c
	$(cc) $(CFLAGS) $(INCLUDES) -c -fPIC ./src/dump.c
	$(cc) $(CFLAGS) $(INCLUDES) -c -fPIC ./src/logging.c
	$(cc) $(CFLAGS) $(INCLUDES) -c -shared -fpic ./src/port_driver.c
	$(cc) $(CFLAGS) $(INCLUDES) -o $(LIB_DIR)liboleg.so murmur3.o logging.o dump.o oleg.o -fpic -shared $(MATH_LINKER)
	$(cc) $(CFLAGS) $(INCLUDES) -L$(LIB_DIR) -o $(LIB_DIR)libolegserver.so port_driver.o -fpic -shared $(MATH_LINKER) -loleg -lei
	$(cc) $(CFLAGS) $(INCLUDES) -c ./src/test.c
	$(cc) $(CFLAGS) $(INCLUDES) -c ./src/main.c
	$(cc) $(CFLAGS) $(INCLUDES) -L$(LIB_DIR) -o $(BIN_DIR)oleg_test test.o main.o $(MATH_LINKER) -loleg

erlang:
	erlc $(ERLFLAGS) ./src/ol_database.erl
	erlc $(ERLFLAGS) ./src/ol_http.erl
	erlc $(ERLFLAGS) ./src/ol_parse.erl
	erlc $(ERLFLAGS) ./src/olegdb.erl

clean:
	rm $(BIN_DIR)*
	rm $(LIB_DIR)*
	rm *.o
