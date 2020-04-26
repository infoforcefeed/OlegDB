#!/usr/bin/env bash

export LD_LIBRARY_PATH=.:$LD_LIBRARY_PATH

rm -rf /tmp/oleg_is_king

CMD=./liboleg.test
if [ $# -eq 0 ]; then
    $CMD
else
    if [ $1 == "gdb" ]; then
        gdb --args $CMD
    elif [ $1 == "valgrind" ]; then
        valgrind --track-fds=yes --track-origins=yes --leak-check=full $CMD
    else
        $CMD
    fi
fi
