#! /bin/bash

echo "Building dLISP interpreter:"
mkdir -p build
FILES=$(echo dle.d dlisp/*.d dlisp/predefs/*.d)

for FILE in $FILES; do
	echo "DMD $FILE"	
	dmd -c $FILE -odbuild
done

dmd build/*.o -ofdle
