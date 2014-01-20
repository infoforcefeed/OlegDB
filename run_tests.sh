#!/bin/bash

export LD_LIBRARY_PATH=./build/lib:$LD_LIBRARY_PATH
exec ./build/bin/oleg_test test
