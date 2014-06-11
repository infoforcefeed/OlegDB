#!/usr/bin/env bash

ATTEMPT=0
handle_close() {
    if [ $ATTEMPT -eq 0 ]; then
        ATTEMPT=1
        #echo "Shutting down oleg..."
        KILLSTRING="{satan, olegdb@$(hostname)} ! {shutdown, self()}, receive _ -> halt() end."
        #echo "Running $KILLSTRING"
        erl +Bd -noinput -sname the_judge_$$ -eval "$KILLSTRING"
        #echo "Shutdown."
    else
        echo "Already tried."
        exit 0
    fi
}
trap handle_close SIGINT SIGTERM

export ERL_MAX_PORTS=4096
RUN_DIR=/run/olegdb
PID_FILE=$RUN_DIR/pid
#TODO: +Bc here might be the trick
ERL_OPTS="+Bi -sname olegdb -noinput +K true -smp enable -run olegdb main ${1+"$@"}"


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
#erl $ERL_OPTS
erl $ERL_OPTS &
ERL_PID=$!
wait $ERL_PID
