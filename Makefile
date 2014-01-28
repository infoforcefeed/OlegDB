CFLAGS=-Wall -Werror -g3
uname_S := $(shell sh -c 'uname -s 2>/dev/null || echo not')
cc=gcc -std=c99
VERSION=0.1
BUILD_DIR=$(shell pwd)/build/
LIB_DIR=$(BUILD_DIR)lib/
BIN_DIR=$(BUILD_DIR)bin/
SONAME=liboleg.so.1
ERLFLAGS=-smp -W1 -Werror -b beam -I./include -o $(BIN_DIR)
ERL_HEADERS=`erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell`

MATH_LINKER=
ifeq ($(uname_S),Darwin)
	# Do nothing
else
	MATH_LINKER=-lm
endif

all: liboleg erlang

liboleg:
	$(cc) $(CFLAGS) -c -fPIC -I./include ./src/murmur3.c
	$(cc) $(CFLAGS) -c -fPIC -I./include ./src/oleg.c
	$(cc) $(CFLAGS) -c -fPIC -I./include ./src/dump.c
	$(cc) $(CFLAGS) $(MATH_LINKER) -shared -Wl,-soname,$(SONAME) -o $(LIB_DIR)liboleg.so.$(VERSION) murmur3.o dump.o oleg.o
	if ! [ -L $(LIB_DIR)$(SONAME) ]; then ln -s $(LIB_DIR)liboleg.so.$(VERSION) $(LIB_DIR)$(SONAME); fi
	if ! [ -L $(LIB_DIR)liboleg.so ]; then ln -s $(LIB_DIR)liboleg.so.$(VERSION) $(LIB_DIR)liboleg.so; fi
	$(cc) $(CFLAGS) -c -I./include ./src/test.c
	$(cc) $(CFLAGS) -c -I./include ./src/main.c
	$(cc) $(CFLAGS) -I./include -L$(LIB_DIR) $(MATH_LINKER) -o $(BIN_DIR)oleg_test test.o main.o -loleg

erlang:
	erlc $(ERLFLAGS) ./src/ol_http.erl
	erlc $(ERLFLAGS) ./src/ol_parse.erl
	erlc $(ERLFLAGS) ./src/olegdb.erl

clean:
	rm $(BIN_DIR)*
	rm $(LIB_DIR)*
	rm *.o
