#!/bin/bash

export PATH=./build/bin/:$PATH
export LD_LIBRARY_PATH=./build/lib/:$LD_LIBRARY_PATH
erl +K true -smp enable -pa ./build/bin -noshell -s olegdb main -s init stop
