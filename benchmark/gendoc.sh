#!/bin/bash

mkdir -p document

FILE=document/benchmark.md
TARGETS="i386-linux-handwritten c"
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

function bootstrap {
    time ./planck < bootstrap.fs benchmark/nop.fs 2>&1 > /dev/null
}

function generate-table {
    echo "## $1"
    echo
    echo "| implementation | sec |"
    echo "|:---------------|----:|"
    for impl in $TARGETS; do
        make $impl 2>&1 > /dev/null
        t=`benchmark $2 $3`
        echo "| $impl | $t |"
    done
}

echo "# Benchmarks" >> $FILE
generate-table "Bootstrap Time" bootstrap 5
