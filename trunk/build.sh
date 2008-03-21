#! /bin/sh

echo
echo "------------------------------------------------------------------------------------------------------------------"
echo

cd src
rm dlisp
ln -s `pwd`/../dlisp_oop/dlisp
rebuild  izedit.d -I$HOME/d/include/d -S$HOME/d/lib/  -I/usr/include/d -S/usr/lib -gc -oq../build  -L-ldl -unittest
rebuild  izplay.d -I$HOME/d/include/d -S$HOME/d/lib/  -I/usr/include/d -S/usr/lib -gc -oq../build  -L-ldl -unittest
cd ..
mv -f src/izedit ./iz-edit
mv -f src/izplay ./iz-play