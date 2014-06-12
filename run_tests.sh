#!/usr/bin/env bash

export LD_LIBRARY_PATH=./build/lib:$LD_LIBRARY_PATH

rm -rf /tmp/oleg_is_king

if [ "$1" == "valgrind" ]
    then
        valgrind --leak-check=full --track-origins=yes ./build/bin/oleg_test
    else
        ./build/bin/oleg_test
fi
