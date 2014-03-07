#!/bin/bash

export ERL_MAX_PORTS=4096
CMD_NAME=$(echo $0|grep -o '[a-zA-Z0-9]\+')
if [ "$CMD_NAME" == 'olegdb' ]
    then
        exec erl -noinput +K true -smp enable -run olegdb main ${1+"$@"}
fi
export PATH=./build/bin/:$PATH
export LD_LIBRARY_PATH=./build/lib/:$LD_LIBRARY_PATH
exec erl -noinput +K true -smp enable -pa ./build/bin -run olegdb main ${1+"$@"}
