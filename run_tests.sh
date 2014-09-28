#!/bin/sh

export LD_LIBRARY_PATH=./build/lib:$LD_LIBRARY_PATH

if [ "$1" == "valgrind" ]
    then
        valgrind --leak-check=full --track-origins=yes ./build/bin/oleg_test
    else
        ./build/bin/oleg_test
fi
