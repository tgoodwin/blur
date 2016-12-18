#!/bin/bash

code(){
    filebase=$(echo ${1} | cut -f 1 -d '.')
	make "${filebase}-ls"
    { ./${filebase}.blx; } &> output.txt
    #cmp --silent output.txt helloWorld.out || echo "Wrong Output"
    DIFF=$(diff -bBw output.txt "${filebase}.out")
    if [ "$DIFF" == "" ]; then
	echo "${filebase}: check"
    else
	echo "${filebase}: Wrong Output"
    fi
    rm -rf tests/*.ll
}

codeAll(){
if [ -f "codeResults.out" ]; then
    rm "codeResults.out"
fi
for i in tests/*.blr
do
    code $i >> codeResults.out;
done
}
