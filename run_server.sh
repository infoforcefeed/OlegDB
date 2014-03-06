#!/bin/bash

export PATH=./build/bin/:$PATH
export LD_LIBRARY_PATH=./build/lib/:$LD_LIBRARY_PATH
export ERL_MAX_PORTS=4096
exec erl -noinput +K true -smp enable -pa ./build/bin -run olegdb main ${1+"$@"}
