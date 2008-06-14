#! /bin/bash

echo "Building dLISP interpreter:"
mkdir -p build
FILES=$(echo dle.d dlisp/*.d dlisp/predefs/*.d)

#EXTRA="-version=debugContext -unittest"
EXTRA=""

for FILE in $FILES; do
	echo "DMD $FILE"	
	dmd -c $FILE -odbuild $EXTRA
done

dmd build/*.o -ofdle -unittest

./dle testcases/validate.lisp

