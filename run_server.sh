#!/usr/bin/env bash

handle_close() {
    echo "WTF"
    HOSTNAME=$(hostname)
    KILLSTRING='{satan, olegdb@'$HOSTNAME'} ! {shutdown, self()}, halt().'
    erl +Bd -noinput -sname the_judge_$$ -eval "$KILLSTRING"
    echo "Should be cleaned."
}
trap handle_close SIGINT

export ERL_MAX_PORTS=4096
RUN_DIR=/run/olegdb
PID_FILE=$RUN_DIR/pid
#TODO: +Bc here might be the trick
ERL_OPTS="+Bc -sname olegdb -noinput +K true -smp enable -run olegdb main ${1+"$@"}"


CMD_NAME=$(basename $0)
if [ "$CMD_NAME" = 'olegdb' ]; then
    if [ -e $PID_FILE ]
        then
            echo "Another instance of OlegDB is already running."
            exit -1
        else
            echo "Starting OlegDB..."
            echo $$ > $PID_FILE
        fi
else
    ERL_OPTS="-pa ./build/bin "$ERL_OPTS
    # DEBUG mode
    export PATH=./build/bin/:$PATH
    export LD_LIBRARY_PATH=./build/lib/:$LD_LIBRARY_PATH
fi

# TODO: Have the trap up above intercept the ctrl+c before this does
exec erl $ERL_OPTS
