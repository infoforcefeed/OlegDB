#!/bin/bash

RUN_DIR=/run/olegdb
PID_FILE=$RUN_DIR/pid

export ERL_MAX_PORTS=4096
CMD_NAME=`basename $0`
if [ "$CMD_NAME" == 'olegdb' ]
    then
        echo $$ > $PID_FILE
        exec erl -noinput +K true -smp enable -run olegdb main ${1+"$@"}
fi
export PATH=./build/bin/:$PATH
export LD_LIBRARY_PATH=./build/lib/:$LD_LIBRARY_PATH
exec erl -noinput +K true -smp enable -pa ./build/bin -run olegdb main ${1+"$@"}
