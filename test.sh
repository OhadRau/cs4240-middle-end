#!/bin/bash
./run.sh

EXP=expected/*ir

for f in $EXP
do
    BASENAME=$(basename $f)
    [[ `diff $f out/$BASENAME` ]] &&
	(echo "$BASENAME: fails (files differ)") ||
	(echo "$BASENAME: passes (files are the same)")
done
	 
