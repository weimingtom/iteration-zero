
cd src
rebuild  -v sdltest.d -I/home/kblindert/d/include/d -S/home/kblindert/d/lib/  -I/usr/include/d -S/usr/lib -oq../build -gc -L-ldl
cd ..

# src/derelict/opengl/glx.d
#bud  src/sdltest.d  -Isrc/ -L-ldl  src/derelict/opengl/glx.d
#mv -f src/sdltest ./
#rm -f *.o
