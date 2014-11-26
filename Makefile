CFLAGS=-Wall -Werror -g3 -O2 -Wstrict-aliasing=2
uname_S := $(shell sh -c 'uname -s 2>/dev/null || echo not')
ifndef CC
	CC = gcc
endif
VERSION=0.1.5
SOVERSION=0
CUR_DIR=$(shell pwd)
BUILD_DIR=$(CUR_DIR)/build/
LIB_DIR=$(BUILD_DIR)lib/
BIN_DIR=$(BUILD_DIR)bin/
PREFIX?=/usr/local
INSTALL_LIB=$(PREFIX)/lib/
INSTALL_BIN=$(PREFIX)/bin/
INSTALL_INCLUDE=$(PREFIX)/include/olegdb/
export CGO_LDFLAGS=-L$(BUILD_DIR)lib

INCLUDES=-I./include

MATH_LINKER=
ifeq ($(uname_S),Darwin)
	# Do nothing
else
	MATH_LINKER=-lm
endif

all: liboleg oleg_test server

test.o: ./c_src/test.c
	$(CC) $(CFLAGS) $(INCLUDES) -c $<

main.o: ./c_src/main.c
	$(CC) $(CFLAGS) $(INCLUDES) -c $<

%.o: ./c_src/%.c
	$(CC) $(CFLAGS) $(INCLUDES) -c -fPIC $<

oleg_test: $(BIN_DIR)oleg_test liboleg
$(BIN_DIR)oleg_test: test.o main.o
	$(CC) $(CFLAGS) $(INCLUDES) -L$(LIB_DIR) -o $(BIN_DIR)oleg_test test.o main.o $(MATH_LINKER) -loleg

liboleg: $(LIB_DIR)liboleg.so
$(LIB_DIR)liboleg.so: murmur3.o oleg.o logging.o aol.o rehash.o file.o utils.o tree.o lz4.o stack.o cursor.o data.o transaction.o
	$(CC) $(CFLAGS) $(INCLUDES) -o $(LIB_DIR)liboleg.so $^ -fpic -shared $(MATH_LINKER)

uninstall:
	rm -rf $(INSTALL_LIB)liboleg*
	rm -rf $(INSTALL_BIN)olegdb

frontend: $(BIN_DIR)olegdb
$(BIN_DIR)olegdb:
	go build -o $(BIN_DIR)olegdb ./frontend/

server: liboleg frontend

install: goinstall

goinstall: server libinstall
	cp $(BIN_DIR)olegdb $(INSTALL_BIN)olegdb

libinstall: liboleg
	@mkdir -p $(INSTALL_LIB)
	@mkdir -p $(INSTALL_INCLUDE)
	install $(LIB_DIR)liboleg.so $(INSTALL_LIB)liboleg.so.$(VERSION)
	ln -fs $(INSTALL_LIB)liboleg.so.$(VERSION) $(INSTALL_LIB)liboleg.so
	ln -fs $(INSTALL_LIB)liboleg.so.$(VERSION) $(INSTALL_LIB)liboleg.so.$(SOVERSION)
	install ./include/*.h $(INSTALL_INCLUDE)

test: liboleg oleg_test
	./run_tests.sh

clean:
	rm -f $(BIN_DIR)*
	rm -f $(LIB_DIR)*
	rm -f *.o
