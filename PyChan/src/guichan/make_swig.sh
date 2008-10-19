
cd guichan
swig -w511 -c++ -python -Iguichan/include -outdir swig_wrapper guichan.i
c++ guichan_wrap.cxx  -I/usr/include/python2.5 -o guichan_wrap.o -c -fPIC -ggdb -O3
c++ -shared guichan/libguichan.so guichan_wrap.o -o swig_wrapper/_guichan.so -Wl,-rpath=.
cd ..

cp -v guichan/swig_wrapper/* guichan/guichan/libguichan* pychan/
cd pychan/; python -c "import guichan; print guichan"; cd ..
