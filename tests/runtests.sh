#!/usr/bin/env bash

parent_path=$( cd "$(dirname "${BASH_SOURCE[0]}")" ; pwd -P )

cd "$parent_path"

mkdir -p bin
mkdir -p out

rm bin/* 2> /dev/null
rm out/* 2> /dev/null

function run_test {
	COLOR=`tput setaf 2`
	f=$1
	TEST_SUCCESS="true"
	NAME=$(basename "$f" .si)
	../build/bin/simon "test/$NAME.si" -o "bin/$NAME"
	if [ $? -ne 0 ]
  	then
    	TEST_SUCCESS="false"
  	fi

    # Test executables
# 	if [ "$TEST_SUCCESS" == "true" ]
# 	then
# 		"bin/$NAME" > "out/$NAME.txt"
# 		if [ $? -ne 0 ]
#   		then
#     		TEST_SUCCESS="false"
#   		fi
#
#         if [ -f "check/$NAME.txt" ]
#         then
#             DIFF=$(diff <(sed 's/(nil)/0x0/g' "out/$NAME.txt") <(sed 's/(nil)/0x0/g' "check/$NAME.txt"))
#             if [ "$DIFF" != "" ]
#             then
#                 TEST_SUCCESS="false"
#             fi
#         else
#             COLOR=`tput setaf 3`
#             echo ${COLOR}$NAME missing check to diff${RESET}
#         fi
# 	fi
	RESET=`tput sgr0`
	if [ "$TEST_SUCCESS" == "false" ]
	then
		COLOR=`tput setaf 1`
	fi
    if [ -t 1 ]; then
    	if [ "$TEST_SUCCESS" == "false" ]; then
            printf "%s%-20s: %s%s\n" $COLOR $NAME FAILED $RESET
        else
            printf "%s%-20s: %s%s\n" $COLOR $NAME passed $RESET
        fi
    else
    	if [ "$TEST_SUCCESS" == "false" ]; then
            printf "%-20s: %s\n" $NAME FAILED
        else
            printf "%-20s: %s\n" $NAME passed
        fi
    fi
}

if [ "$#" -eq 0 ]
then
#     N_PROC=$(ls test/*.si | wc | awk '{ print $1; }')
    N_PROC=1
    export -f run_test
    ls test/*.si | xargs -L 1 -P "$N_PROC" -I FILE bash -c "run_test FILE"
else
	run_test test/$1.si
fi



##
