#!/bin/bash

# bash script to read & test text files in tests folder through LLVM

# Path to LLVM
#LLI="lli"

# BLUR compiler
#BLUR="./blur.native"

# operation time limit
#ulimit -t 30

#Usage() {
#    echo "Usage: testall.sh"
#    echo "-s    Test single case"
#    echo "-a    Test all"
#    exit 1
#}

#SignalError() {
#    if [ $error -eq 0 ] ; then
#	echo "FAILED"
#	error=1
#    fi
#    echo " $1"
#}

# Compare <file1> <file2> <diff>
# compares file1 and file2, writes difference to diff

#Compare() {
#    generated="$generated $3"
#    echo diff -b $1 $2 ">" $3 1>&2
#    diff -b "$1" "$2" > "$3" 2>$1 || {
#	SignalError "$1 differs"
#	echo "FAILED $1 differs from $2 " 1>&2

#    }

#}

check(){
#filebase=$(basename "${1}" ".blr")
filebase=$(echo ${1} | cut -f 1 -d '.')
if [ "$#" -ne 1 ]; then
    echo "Usage: check filename"
else
    diff "${filebase}.blr" "${filebase}.blr.pp"
fi
}
