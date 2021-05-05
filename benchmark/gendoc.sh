#!/bin/bash

COMMIT="$1"
TARGETS="i386-linux-handwritten c python"
TIMEFORMAT='%U'
CPU_MODEL=`cat /proc/cpuinfo | grep -m1 'model name' | cut -d: -f2 | sed "s/^ *//g"`
MEM_SIZE="`cat /proc/meminfo | grep 'MemTotal' | awk '{ print $2/1024/1024 }'` GB"
UNAME=`uname -a`

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
    echo "## $1"
    echo "\`$2\`"
    echo
    #echo "Average of $3 execution times."
    #echo
    echo "| runtime implementation | execution time (sec) |"
    echo "|:-----------------------|---------------------:|"
    for impl in $TARGETS; do
        make $impl 2>&1 > /dev/null
        runtime=`./planck < bootstrap.fs --runtime`
        t=`benchmark "time $2 2>&1 > /dev/null" $3`
        echo "| $runtime | $t |"
    done
    echo
}

echo "# Environment"
echo
echo "- Commit: $COMMIT"
echo "- $CPU_MODEL"
echo "- $MEM_SIZE"
echo "- $UNAME"
echo

echo "# Benchmarks"

generate-table "Bootstrap Time" "./planck < bootstrap.fs benchmark/nop.fs" 1
generate-table "Fib(30)" "./planck < bootstrap.fs benchmark/fib.fs" 1
generate-table "Matmul" "./planck < bootstrap.fs benchmark/matmul.fs" 1
