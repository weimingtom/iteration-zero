
cd src
rebuild  sdltest.d -I/home/kblindert/d/include/d -S/home/kblindert/d/lib/  -I/usr/include/d -S/usr/lib -oq../build  -L-ldl
cd ..
mv -f src/sdltest ./
