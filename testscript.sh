#!/bin/bash
# authored by Daniel Hong sh3266@columbia.edu

code(){
    filebase=$(echo ${1} | cut -f 1 -d '.')
    { ./blur -ls < "${filebase}.blr" > "${filebase}.ll"; } &> output.txt
	if [ -s output.txt ]; then
		:
	else
    	make "${filebase}-ls" &> garb.txt # now have an executable with .blx extension
    	./${filebase}.blx &> output.txt
		#echo "checking" >> output.txt
	fi
    #cmp --silent output.txt helloWorld.out || echo "Wrong Output"
    DIFF=$(diff -bBw output.txt "${filebase}.out")
    if [ "$DIFF" == "" ]; then
	echo "${filebase}: check"
    else
	echo "${filebase}: Wrong Output"
    fi
    rm -rf tests/*.ll
	rm -rf tests/*.s
	rm -rf tests/*.blx
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
