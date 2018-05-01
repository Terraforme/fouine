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
	#cat $test/$tests
	fouine=$(./main.native $tests/$test 2> /dev/null)
	caml=$(cat $prInt $tests/$test | ocaml -w -26 -stdin 2> /dev/null)
	# parce que omg ce warning 26 qui me casse les pieds !!!
	

	if [ "$fouine" != "$caml" ]
	then
		$becho -e "\n\e[31;1m$test\e[0m"
		cat $tests/$test
		
		$becho -n -e "\n\e[32;1mFouine:\t\e[0m" ; echo $(./main.native $tests/$test 2> /dev/null)					
		$becho -n -e "\e[32;1mOCamL :\t\e[0m"   ; echo $(cat $prInt $tests/$test | ocaml -w -26 -stdin 2> /dev/null) 

		$becho -e "\e[33;1mMore details\n· Fouine :\e[0m"
		./main.native $tests/$test
		$becho -e "\e[33;1m· OCamL :\e[0m"
		cat $prInt $tests/$test | ocaml -stdin
		echo ""
	fi
done;


$becho -e "\n""\e[34;1m"Starting Tests"\e[0m" "\n"

$becho -e "\e[33;1mERP\e[0m"
for test in $(ls $tests)
do
	#cat $tests/$test
	
	cfouine=$(./main.native $tests/$test -E 2> /dev/null)  #continuation
	caml=$(cat $prInt $tests/$test | ocaml -w -26 -stdin 2> /dev/null)
	# parce que omg ce warning 26 qui me casse les pieds !!!
	
	if [ "$cfouine" = "$caml" ]
	then 
	  $becho -e -n " "
	else
	  $becho -e -n "\e[33;1mE\e[0m"
	fi
	
	rfouine=$(./main.native $tests/$test -R 2> /dev/null)  #sans références
	if [ "$rfouine" = "$caml" ]
	then
	  $becho -e -n " "
	else
	  $becho -e -n "\e[33;1mR\e[0m"
	fi
	
	mfouine=$(./main.native $tests/$test -machine 2> /dev/null) #machine secd
	if [ "$mfouine" = "$caml" ]
	then 
	  $becho -e -n " "
	else 
	  $becho -e -n "\e[33;1mM\e[0m"
	fi
	
	fouine=$(./main.native $tests/$test 2> errors.log)  #normal
	error=$(cat errors.log)
	if [ "$error" = "Fatal error: exception Parsing.Parse_error" ]
	then
		$becho -e -n "\e[33;1mP\e[0m"
	else
		$becho -e -n " " 
	fi 

	if [ "$fouine" = "$caml" ]
	then
		$becho -e "\e[32;1m✓\e[0m  $test"
	else
		$becho -e "\e[31;1m✕  $test\e[0m" 
	fi
done;

