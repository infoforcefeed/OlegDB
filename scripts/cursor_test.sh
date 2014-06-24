#!/bin/bash


# This script doesn't really do any testing, it just does some inserts
# and pulls the items back out again.

LETTERS=(a b c d e f g h i j k l m n o p q r s t u v w x y z)
for I in $(seq 0 25); do
    VAL=${LETTERS[$I]}
    ITER=$VAL'test'$VAL
    RESP=$(curl -X POST -d $ITER localhost:8080/oleg/$ITER 2> /dev/null)
done

for I in $(seq 0 25); do
    VAL=${LETTERS[$I]}
    ITER=$VAL'test'$VAL
    RESP=$(curl localhost:8080/oleg/$ITER 2> /dev/null)
    echo "$ITER: $RESP"
done

for I in $(seq 0 25); do
    VAL=${LETTERS[$I]}
    ITER=$VAL'test'$VAL
    RESP=$(curl localhost:8080/oleg/$ITER/_next 2> /dev/null)
    echo "Item after key $ITER is $RESP"
done
