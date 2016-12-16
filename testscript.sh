#!/bin/bash

check(){
#filebase=$(basename "${1}" ".blr")
filebase=$(echo ${1} | cut -f 1 -d '.')
#if [ "$#" -ne 1 ]; then
#    echo "Usage: check filename"
#else
#    diff "${filebase}.blr" "${filebase}.blr" # .blr.pp
#    echo "${filebase} check: checked! "
#    cmp --silent "${filebase}.blr" "${filebase}.blr" || echo "Wrong Output"
#fi
echo "${filebase} pretty print: "
./prog -p < "${filebase}.blr"
}

testAll(){
#rm results.out
for i in tests/*.blr
do
    check $i >> results.out;
done
}

code(){
    filebase=$(echo ${1} | cut -f 1 -d '.')
    { ./prog -l < "${filebase}.blr" > "${filebase}.ll" && lli "${filebase}.ll"; } &> output.txt
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
