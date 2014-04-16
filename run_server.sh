#!/usr/bin/env bash

export ERL_MAX_PORTS=4096
RUN_DIR=/run/olegdb
PID_FILE=$RUN_DIR/pid
ERL_OPTS="+Bd -noinput +K true -smp enable -run olegdb main ${1+"$@"}"

CMD_NAME=`basename $0`
if [ "$CMD_NAME" == 'olegdb' ]; then
    if [ -e $PID_FILE ]
        then
            echo "Another instance of OlegDB is already running."
            exit -1
        else
            echo "Starting OlegDB..."
            echo $$ > $PID_FILE
            exec erl $ERL_OPTS
        fi
else
    # DEBUG mode
    export PATH=./build/bin/:$PATH
    export LD_LIBRARY_PATH=./build/lib/:$LD_LIBRARY_PATH
    exec erl -pa ./build/bin $ERL_OPTS
fi
