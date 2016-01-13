#!/bin/sh
# 
# This script helps with the compiling of source files.
# It would compile all the source files in current directory, unless otherwise specified.
# 

# compiler
# CC=scalac
# Fast Scala Compiler
CC=fsc

# the compiling output directory
OUT=target


if [ ! -d $OUT ]; then
    mkdir $OUT
fi

if [ $# -gt 0 ]; then
    SRC=$@
else
    SRC=`ls *.scala`
fi


cd $OUT

# compile the dependency sources first.
$CC ../utils.scala


for file in $SRC; do
    echo "compile $file ..."
    $CC ../$file
done


