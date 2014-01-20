#!/bin/bash

export PATH=./build/bin/:$PATH
export LD_LIBRARY_PATH=./build/lib/:$LD_LIBRARY_PATH
erl -pa ./build/bin -noshell -s olegdb main -s init stop
