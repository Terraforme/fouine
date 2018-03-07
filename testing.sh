#!/bin/sh

tests="Tests/"
sol_dir="no sol dir"
prInt="prInt.test"

make


echo "\n **************** Starting Tests *****************"
for test in $(ls $tests)
do
	printf "Testing %-10s :\n" $test
	#cat $tests/$test
	fouine= echo $(./main.native $tests/$test)
	caml= echo $(cat $prInt $tests/$test | ocaml -w -26 -stdin )
	# parce que omg ce warning 26 qui me casse les pieds !!!
	

	if [ "-"$fouine = "-"$caml ]
	then
		echo "\t---- OK\n"
	else
		echo "\t---- not OK\n"
	fi
done;

