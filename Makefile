CFLAGS=-Wall -Werror -g3 -O2 -Wstrict-aliasing=2
uname_S := $(shell sh -c 'uname -s 2>/dev/null || echo not')
ifndef CC
	CC = gcc
endif
VERSION=0.1.5
SOVERSION=0
BUILD_DIR=$(shell pwd)/build/
LIB_DIR=$(BUILD_DIR)lib/
BIN_DIR=$(BUILD_DIR)bin/
PREFIX?=/usr/local
INSTALL_LIB=$(PREFIX)/lib/
INSTALL_BIN=$(PREFIX)/bin/
INSTALL_INCLUDE=$(PREFIX)/include/olegdb/

TEST_OUT=$(BIN_DIR)oleg_test
LIB_OUT=$(LIB_DIR)liboleg.so
BIN_OUT=$(BIN_DIR)olegdb
export CGO_LDFLAGS=-L$(BUILD_DIR)lib

INCLUDES=-I./include

MATH_LINKER=
ifeq ($(uname_S),Darwin)
	# Do nothing
else
	MATH_LINKER=-lm
endif

all: $(TEST_OUT) $(BIN_OUT)

test.o: ./c_src/test.c
	$(CC) $(CFLAGS) $(INCLUDES) -c $<

main.o: ./c_src/main.c
	$(CC) $(CFLAGS) $(INCLUDES) -c $<

%.o: ./c_src/%.c
	$(CC) $(CFLAGS) $(INCLUDES) -c -fPIC $<

oleg_test: $(TEST_OUT)
$(TEST_OUT): $(LIB_OUT) test.o main.o
	$(CC) $(CFLAGS) $(INCLUDES) -L$(LIB_DIR) -o $(TEST_OUT) test.o main.o $(MATH_LINKER) -loleg

liboleg: $(LIB_OUT)
$(LIB_OUT): murmur3.o oleg.o logging.o aol.o rehash.o file.o utils.o tree.o lz4.o stack.o cursor.o data.o transaction.o
	$(CC) $(CFLAGS) $(INCLUDES) -o $(LIB_OUT) $^ -fpic -shared $(MATH_LINKER)

uninstall:
	rm -rf $(INSTALL_LIB)liboleg*
	rm -rf $(INSTALL_BIN)olegdb

$(BIN_OUT): $(LIB_OUT)
	go build -o $(BIN_OUT) ./frontend/

install: goinstall

goinstall: $(BIN_OUT) libinstall
	cp $(BIN_OUT) $(INSTALL_BIN)olegdb

libinstall: $(LIB_OUT)
	@mkdir -p $(INSTALL_LIB)
	@mkdir -p $(INSTALL_INCLUDE)
	install $(LIB_OUT) $(INSTALL_LIB)liboleg.so.$(VERSION)
	ln -fs $(INSTALL_LIB)liboleg.so.$(VERSION) $(INSTALL_LIB)liboleg.so
	ln -fs $(INSTALL_LIB)liboleg.so.$(VERSION) $(INSTALL_LIB)liboleg.so.$(SOVERSION)
	install ./include/*.h $(INSTALL_INCLUDE)

test: $(LIB_OUT) $(TEST_OUT)
	./run_tests.sh

clean:
	rm -f $(BIN_DIR)*
	rm -f $(LIB_DIR)*
	rm -f *.o
