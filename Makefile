VERSION=0.1.6
SOVERSION=0
NAME=liboleg.so
BIN=olegdb

CFLAGS=-Wall -Werror -g -O2 -Wstrict-aliasing=2 -Wno-format-truncation
SOFLAGS=-Wl,-soname,$(NAME).$(SOVERSION)
OBJ_FILES=vector.o murmur3.o oleg.o logging.o aol.o rehash.o file.o utils.o tree.o lz4.o stack.o cursor.o transaction.o

uname_S := $(shell sh -c 'uname -s 2>/dev/null || echo not')

DESTDIR?=
PREFIX?=${DESTDIR}/usr
INSTALL_LIB=$(PREFIX)/lib/
INSTALL_BIN=$(PREFIX)/bin/
INSTALL_INCLUDE=$(PREFIX)/include/olegdb/

TEST_OUT=liboleg.test
LIB_OUT=$(NAME).$(VERSION)
STATIC_LIB_OUT=liboleg.a

INCLUDES=-I./include

ifndef CC
	CC = gcc
endif

MATH_LINKER=-lm

all: $(TEST_OUT) $(BIN)

test.o: ./src/test.c
	$(CC) $(CFLAGS) $(INCLUDES) -c $<

main.o: ./src/main.c
	$(CC) $(CFLAGS) $(INCLUDES) -c $<

%.o: ./src/%.c
	$(CC) $(CFLAGS) $(INCLUDES) -c -fPIC $<

$(TEST_OUT): liboleg test.o main.o
	$(CC) $(INCLUDES) -L$(PWD) -o $(TEST_OUT) test.o main.o $(MATH_LINKER) -loleg

liboleg: $(LIB_OUT)
$(LIB_OUT): $(OBJ_FILES)
	$(CC) $(INCLUDES) $(SOFLAGS) -o $(LIB_OUT) $^ -fpic -shared $(MATH_LINKER)
	ln -s $(LIB_OUT) $(NAME)
	ln -s $(LIB_OUT) $(NAME).$(SOVERSION)

static: $(LIB_OUT) $(OBJ_FILES)
	ar rcs $(STATIC_LIB_OUT) $(OBJ_FILES)

uninstall:
	rm -rf $(INSTALL_LIB)liboleg*
	rm -rf $(INSTALL_BIN)olegdb
	rm -rf $(INSTALL_INCLUDE)

$(BIN): liboleg $(LIB_OUT)
	CGO_LDFLAGS=-L$(PWD) go build -o $(BIN) ./cmd/olegdb

install: goinstall

goinstall: olegdb libinstall
	@install -D $(BIN) $(INSTALL_BIN)olegdb

libinstall: liboleg
	@mkdir -p $(INSTALL_LIB)
	@mkdir -p $(INSTALL_INCLUDE)
	@install $(LIB_OUT) $(INSTALL_LIB)$(NAME).$(VERSION)
	@ln -fsr $(INSTALL_LIB)$(NAME).$(VERSION) $(INSTALL_LIB)$(NAME)
	@ln -fsr $(INSTALL_LIB)$(NAME).$(VERSION) $(INSTALL_LIB)$(NAME).$(SOVERSION)
	@install ./include/*.h $(INSTALL_INCLUDE)
	@echo "OlegDB installed to $(PREFIX) :^)."

test: $(LIB_OUT) $(TEST_OUT)
	./run_tests.sh

clean:
	rm -f $(BIN)
	rm -f $(NAME)
	rm -f $(NAME).$(SOVERSION)
	rm -f $(NAME).$(VERSION)
	rm -f $(TEST_OUT)
	rm -f *.o

.PHONY: clean test libinstall install uninstall static all
