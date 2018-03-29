#!/bin/sh

tests="Tests/"
sol_dir="no sol dir"
prInt="preludeCaml.ml"
becho=$(which echo)

debug=false
if [ "$1" = "-d" ]
then
	debug=true
fi

$becho -e "\e[34;1mCompilation\e[0m\n"
make


$becho -e "\n\e[34;1mDebug\e[0m" 
for test in $(ls $tests)
do
	#cat $tests/$test
	fouine=$(./main.native $tests/$test 2> /dev/null)
	caml=$(cat $prInt $tests/$test | ocaml -w -26 -stdin 2> /dev/null)
	# parce que omg ce warning 26 qui me casse les pieds !!!
	

	if [ "$fouine" != "$caml" ]
	then
		$becho -e "\n\e[31;1m$test\e[0m"
		cat $tests/$test
		echo ""
		$becho -e "\e[32;1mFouine:\t\e[37;0m$fouine\e[0m"
		$becho -e "\e[32;1mOCamL :\t\e[37;0m$caml\e[0m\n"
		cat $prInt $tests/$test | ocaml -stdin
		echo ""
	fi
done;


$becho -e "\n""\e[34;1m"Starting Tests"\e[0m" "\n"
for test in $(ls $tests)
do
	#cat $tests/$test
	fouine=$(./main.native $tests/$test 2> /dev/null)
	caml=$(cat $prInt $tests/$test | ocaml -w -26 -stdin 2> /dev/null)
	# parce que omg ce warning 26 qui me casse les pieds !!!
	

	if [ "$fouine" = "$caml" ]
	then
		$becho -e "\e[32;1m✓ \e[0m $test" 
	else
		$becho -e "\e[31;1m✕  $test\e[0m" 
	fi
done;

