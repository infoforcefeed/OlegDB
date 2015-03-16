#!/usr/bin/env bash

export LD_LIBRARY_PATH=./build/lib:$LD_LIBRARY_PATH

rm -rf /tmp/oleg_is_king

CMD=./build/bin/oleg_test
if [ $# -eq 0 ]; then
    $CMD
else
    if [ $1 == "gdb" ]; then
        gdb --args $CMD
    elif [ $1 == "valgrind" ]; then
        valgrind --track-origins=yes --leak-check=full $CMD
    else
        $CMD
    fi
fi
