#!/bin/sh
# Usage:
#    > ./run.sh source_file 
#
# e.g. ./run.sh page_rank.scala


RUN=scala

LD_FLAG="-classpath target"

INPUT_DIR=test

CMD_OPTS=""

if [ $# -gt 0 ]; then
    SRC_FILE=$1
    options=($@)
    unset options[0]
    CMD_OPTS=${options[@]}  # the command options other than the input file.
else
    SRC_FILE=`ls *.scala`
fi

for file in $SRC_FILE; do

    # replace the extension
    INPUT_FILE=${file//.scala/.input}
    if ! [ -e $INPUT_DIR/$INPUT_FILE ]
    then
        INPUT_FILE=${file//.scala/.csv}
    fi

    # extract the main object from the source file
    MAIN_OBJ=`grep -E "object .*{" $file | cut -d " " -f 2`

    echo "$RUN $LD_FLAG $MAIN_OBJ $INPUT_DIR/$INPUT_FILE $CMD_OPTS"
    $RUN $LD_FLAG $MAIN_OBJ $INPUT_DIR/$INPUT_FILE $CMD_OPTS

done



