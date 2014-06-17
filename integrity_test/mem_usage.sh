#!/usr/bin/env bash

function get_usage() {
    J=0
    for I in $(cat /proc/$1/smaps | grep '^Size:' | awk '{print $2}'); 
    do
        ((J=J+$I))
    done
    echo -n $J' kB'
}

while true;
do
    echo $(($(date +%s%N)/1000000)) $(get_usage $1)
done
