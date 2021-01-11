#!/bin/bash

TARGETS="i386-linux-handwritten c python"
TIMEFORMAT='%U'
function benchmark () {
    sum=0
    for i in `seq $2`; do
        t=`{ eval $1; } 2>&1`
        sum=`echo $t + $sum | bc`
    done
    average=`echo "scale=3; $sum / $2" | bc | xargs printf "%.3f"`
    echo $average
}

function generate-table {
    echo "# $1"
    echo "\`$2\`"
    echo
    #echo "Average of $3 execution times."
    #echo
    echo "| implementation | sec |"
    echo "|:---------------|----:|"
    for impl in $TARGETS; do
        make $impl 2>&1 > /dev/null
        t=`benchmark "time $2 2>&1 > /dev/null" $3`
        echo "| $impl | $t |"
    done
    echo
}

generate-table "Bootstrap Time" "./planck < bootstrap.fs benchmark/nop.fs" 1
generate-table "Fib(30)" "./planck < bootstrap.fs benchmark/fib.fs" 1
