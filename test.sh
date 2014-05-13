#!/bin/bash

KOKA=koka
EXIT=0

function ok {
	printf "\x1b[32;1m[OK]\x1b[0m      $1\n"
}

function fail {
	printf "\x1b[31;1m[FAIL]\x1b[0m    $1\n"
	EXIT=1
}

function test_module {
	cmd="$KOKA -c --console=raw -itest --outdir=out/test $FLAGS ./test/$1.kk"
	out=$($cmd)
	diff=$(printf "$out" | diff -b ./test/$1.kk.out -)
	if [ $? == 0 ]; then
		ok   $1
	else
		fail $1
		echo   "$cmd"
		printf "$diff"
		echo
	fi
}

#FIXME: tests require properly installed nodejs with dependencies
FLAGS="--showtypesigs -e"

#test_module medium/collatz
#test_module medium/fibonacci
#test_module medium/garcia-wachs
#test_module medium/gcd
#test_module medium/nqueens

FLAGS="-l "

test_module cgen/wrong/assign1
test_module cgen/wrong/rec2
test_module cgen/assign1
test_module cgen/higherkind0
test_module cgen/higherkind1
test_module cgen/higherkind2
test_module cgen/higherkind3
test_module cgen/javascript
test_module cgen/rec1
test_module cgen/rec2
test_module cgen/rec3
test_module cgen/rec4
test_module cgen/rec5

FLAGS="-l --showkindsigs"

test_module kind/wrong/alias1
test_module kind/wrong/negative2
test_module kind/wrong/rec1
test_module kind/wrong/rec2
test_module kind/wrong/type1
test_module kind/wrong/type2
test_module kind/wrong/type3
test_module kind/wrong/type4
test_module kind/wrong/type7
test_module kind/alias1
test_module kind/alias2
test_module kind/alias3
test_module kind/alias4
test_module kind/bgroup1
test_module kind/bgroup2
test_module kind/bgroup3
test_module kind/fix1
test_module kind/fix1a
test_module kind/fix2
test_module kind/type1
test_module kind/type2
test_module kind/type2
test_module kind/type3
test_module kind/type4
test_module kind/type5
test_module kind/type6
test_module kind/type7
test_module kind/type8
test_module kind/type9
test_module kind/type10

FLAGS="-l --showtypesigs --showkindsigs"

test_module static/wrong/alias1
test_module static/wrong/case1
test_module static/wrong/case2
test_module static/wrong/case3
test_module static/wrong/case4
test_module static/wrong/case5
test_module static/wrong/case6
test_module static/wrong/duplicate1
test_module static/wrong/duplicate2
test_module static/wrong/duplicate3
test_module static/wrong/duplicate3a
test_module static/wrong/duplicate3b
test_module static/wrong/module1
test_module static/wrong/module2
test_module static/wrong/rec1
test_module static/wrong/shadow1
test_module static/wrong/shadow2
test_module static/wrong/shadow3
test_module static/wrong/shadow4

test_module static/div1
test_module static/div2-ack
test_module static/div3
test_module static/modA
test_module static/modAWrong
test_module static/modB
test_module static/module1
test_module static/module1a
test_module static/recursive1
test_module static/shadow3a
test_module static/shadow3b
test_module static/xcase1

FLAGS="-l"

test_module syntax/comment1
test_module syntax/layout1
test_module syntax/layout2
test_module syntax/syntax1
test_module syntax/type1
test_module syntax/utf1
test_module syntax/utf2

test_module syntax/wrong/utf1

FLAGS="-l --showtypesigs"

test_module type/eff1
test_module type/eff2
test_module type/eff3
test_module type/eff4a
test_module type/eff4b
test_module type/eff4c
test_module type/eff4
test_module type/eff5a
test_module type/eff5
test_module type/eff6a
test_module type/eff6b
test_module type/eff6
test_module type/eff7a
test_module type/eff7
test_module type/eff8a
test_module type/eff8b
test_module type/eff8
test_module type/eff9
test_module type/higherrank2
test_module type/higherrank3
test_module type/hm1
test_module type/hm2
test_module type/hr1a
test_module type/hr1
test_module type/hr2
test_module type/hr3a
test_module type/hr3b
test_module type/hr3c
test_module type/hr3
test_module type/hr6
test_module type/hr7
test_module type/match1
test_module type/talpin-jouvelot1
test_module type/warn1

test_module type/wrong/div1
test_module type/wrong/div2
test_module type/wrong/div3
test_module type/wrong/higherrank1
test_module type/wrong/higherrank2
test_module type/wrong/higherrank3a
test_module type/wrong/higherrank3
test_module type/wrong/higherrank3v
test_module type/wrong/hm1
test_module type/wrong/hm2
test_module type/wrong/hm3
test_module type/wrong/hr1
test_module type/wrong/hr2
test_module type/wrong/hr3
test_module type/wrong/hr4
test_module type/wrong/hr5
test_module type/wrong/hr6
test_module type/wrong/hr7a
test_module type/wrong/hr7
test_module type/wrong/overlap1
test_module type/wrong/overlap2
test_module type/wrong/overlap3
test_module type/wrong/st1

exit $EXIT