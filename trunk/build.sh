#! /bin/sh

echo
echo "------------------------------------------------------------------------------------------------------------------"
echo

cd src
rebuild  izedit.d -I/home/kblindert/d/include/d -S/home/kblindert/d/lib/  -I/usr/include/d -S/usr/lib -gc -oq../build  -L-ldl -unittest
rebuild  izplay.d -I/home/kblindert/d/include/d -S/home/kblindert/d/lib/  -I/usr/include/d -S/usr/lib -gc -oq../build  -L-ldl -unittest
cd ..
mv -f src/izedit ./iz-edit
mv -f src/izplay ./iz-play
