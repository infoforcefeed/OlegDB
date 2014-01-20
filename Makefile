CFLAGS=-Wall -Werror -g3
uname_S := $(shell sh -c 'uname -s 2>/dev/null || echo not')
cc=gcc -std=c99
VERSION=0.1
BUILD_DIR=$(shell pwd)/build/
LIB_DIR=$(BUILD_DIR)lib/
BIN_DIR=$(BUILD_DIR)bin/
SONAME=liboleg.so.1

MATH_LINKER=
ifeq ($(uname_S),Darwin)
	# Do nothing
else
	MATH_LINKER=-lm
endif

all:
	$(cc) $(CFLAGS) -c -fPIC -I./include ./src/murmur3.c
	$(cc) $(CFLAGS) -c -fPIC -I./include ./src/oleg.c
	$(cc) $(CFLAGS) -c -fPIC -I./include ./src/test.c
	$(cc) $(CFLAGS) -c -fPIC -I./include ./src/main.c
	$(cc) $(CFLAGS) $(MATH_LINKER) -shared -Wl,-soname,$(SONAME) -o $(LIB_DIR)liboleg.so.$(VERSION) murmur3.o oleg.o
	$(cc) $(CFLAGS) $(MATH_LINKER) -L$(LIB_DIR) -l:liboleg.so.$(VERSION) -o $(BIN_DIR)oleg_test murmur3.o oleg.o test.o main.o
	if ! [ -L $(LIB_DIR)$(SONAME) ]; then ln -s $(LIB_DIR)liboleg.so.$(VERSION) $(LIB_DIR)$(SONAME); fi
	# Erlang stuff
	erlc -smp -W1 -Werror -b beam -I./include -o $(BIN_DIR) ./src/olegdb.erl

clean:
	rm $(BIN_DIR)*
	rm $(LIB_DIR)*
	rm *.o
