#!/bin/sh

tests="Tests/"
sol_dir="no sol dir"

make


echo "\n **************** Starting Tests *****************"
for test in $(ls $tests)
do
	printf "********************************************\n"
	printf "Testing %-10s :\n" $test
	cat $tests/$test
	printf "\nResults :\n"
	./main.native $tests/$test
	echo "\n\n\n"

done;

