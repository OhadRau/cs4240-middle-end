#!/bin/bash
set -e

IN=examples/*ir

for f in $IN
do
	BASENAME=$(basename $f)
	dune exec src/optimize.exe -- -i $f -o out/$BASENAME
done
