CFLAGS=-Wall -Werror -g3 -O2
uname_S := $(shell sh -c 'uname -s 2>/dev/null || echo not')
ifndef CC
	CC = gcc
endif
VERSION=0.1.0
SOVERSION=0
BUILD_DIR=$(shell pwd)/build/
LIB_DIR=$(BUILD_DIR)lib/
BIN_DIR=$(BUILD_DIR)bin/
PREFIX?=/usr/local
INSTALL_LIB=$(PREFIX)/lib/
INSTALL_BIN=$(PREFIX)/bin/
INSTALL_INCLUDE=$(PREFIX)/include/

ERL_LIB_LOOKFOR=-DLIBLOCATION=\"./build/lib/\"
ERLFLAGS=-smp -W1 -Werror -b beam -I./include -o $(BIN_DIR) $(ERL_LIB_LOOKFOR)
ERL_DIR=$(shell echo 'io:format("~s~n",[code:root_dir()]),init:stop().' | erl | sed -n '/^1>/s/^1> //p')
ERLI_DIR=$(shell echo 'io:format("~s~n",[code:lib_dir(erl_interface)]),init:stop().' | erl | sed -n '/^1>/s/^1> //p')
ERLINCLUDES=-I$(ERL_DIR)/usr/include/ -I$(ERLI_DIR)/include/
ERLLIBS=-L$(ERL_DIR)/usr/lib/ -L$(ERLI_DIR)/lib/
ERL_ODB_INSTALL_DIR=$(ERL_DIR)/lib/olegdb-$(VERSION)
INCLUDES=-I./include $(ERLINCLUDES)

MATH_LINKER=
ifeq ($(uname_S),Darwin)
	# Do nothing
else
	MATH_LINKER=-lm
endif

all: liboleg oleg_test server

test.o: ./src/test.c
	$(CC) $(CFLAGS) $(INCLUDES) -c $<
main.o: ./src/main.c
	$(CC) $(CFLAGS) $(INCLUDES) -c $<

%.o: ./src/%.c
	$(CC) $(CFLAGS) $(INCLUDES) -c -fPIC $<

FORCE:

$(BIN_DIR)ol_database.beam: ./src/ol_database.erl FORCE
	erlc $(ERLFLAGS) $<

$(BIN_DIR)%.beam: ./src/%.erl
	erlc $(ERLFLAGS) $<

oleg_test: liboleg $(BIN_DIR)oleg_test
$(BIN_DIR)oleg_test: test.o main.o
	$(CC) $(CFLAGS) $(INCLUDES) -L$(LIB_DIR) -o $(BIN_DIR)oleg_test test.o main.o $(MATH_LINKER) -loleg

liboleg: $(LIB_DIR)liboleg.so
$(LIB_DIR)liboleg.so: murmur3.o oleg.o dump.o logging.o aol.o port_driver.o rehash.o utils.o tree.o
	$(CC) $(CFLAGS) $(INCLUDES) -o $(LIB_DIR)liboleg.so murmur3.o logging.o dump.o aol.o oleg.o tree.o rehash.o utils.o -fpic -shared $(MATH_LINKER)
	$(CC) $(CFLAGS) $(INCLUDES) $(ERLLIBS) -L$(LIB_DIR) -o $(LIB_DIR)libolegserver.so port_driver.o -fpic -shared $(MATH_LINKER) -loleg -lei

server: $(BIN_DIR)ol_database.beam $(BIN_DIR)ol_http.beam \
	$(BIN_DIR)ol_parse.beam $(BIN_DIR)ol_util.beam $(BIN_DIR)olegdb.beam

uninstall:
	rm -rf $(INSTALL_LIB)/liboleg*
	rm -rf $(INSTALL_BIN)/olegdb
	rm -rf $(ERL_ODB_INSTALL_DIR)

install: ERL_LIB_LOOKFOR=-DLIBLOCATION=\"$(INSTALL_LIB)\"
install: liboleg server
	@mkdir -p $(INSTALL_LIB)
	@mkdir -p $(INSTALL_BIN)
	@mkdir -p $(INSTALL_INCLUDE)
	install $(LIB_DIR)liboleg.so $(INSTALL_LIB)liboleg.so.$(VERSION)
	ln -fs $(INSTALL_LIB)liboleg.so.$(VERSION) $(INSTALL_LIB)liboleg.so
	ln -fs $(INSTALL_LIB)liboleg.so.$(VERSION) $(INSTALL_LIB)liboleg.so.$(SOVERSION)
	install $(LIB_DIR)libolegserver.so $(INSTALL_LIB)libolegserver.so.$(VERSION)
	ln -fs $(INSTALL_LIB)libolegserver.so.$(VERSION) $(INSTALL_LIB)libolegserver.so
	ln -fs $(INSTALL_LIB)libolegserver.so.$(VERSION) $(INSTALL_LIB)libolegserver.so.$(SOVERSION)
	install ./include/*.h $(INSTALL_INCLUDE)
	@mkdir -p $(ERL_ODB_INSTALL_DIR)/src
	@mkdir -p $(ERL_ODB_INSTALL_DIR)/include
	@mkdir -p $(ERL_ODB_INSTALL_DIR)/ebin
	install $(BIN_DIR)*.beam $(ERL_ODB_INSTALL_DIR)/ebin
	install ./include/*.hrl $(ERL_ODB_INSTALL_DIR)/include
	install ./src/*.erl $(ERL_ODB_INSTALL_DIR)/src
	install ./src/*.app.src $(ERL_ODB_INSTALL_DIR)/ebin
	cp ./run_server.sh $(INSTALL_BIN)olegdb

test: all
	./run_tests.sh

clean:
	rm -f $(BIN_DIR)*
	rm -f $(LIB_DIR)*
	rm -f *.o
