#! /bin/sh

REV=-r1148

mkdir -p guichan
svn checkout $REV http://guichan.googlecode.com/svn/trunk/src/ guichan/src
svn checkout $REV http://guichan.googlecode.com/svn/trunk/include/ guichan/include
svn checkout $REV http://guichan.googlecode.com/svn/trunk/CMake/ guichan/CMake
# svn cat $REV http://guichan.googlecode.com/svn/trunk/CMakeLists.txt > guichan/CMakeLists.txt

cd guichan
cmake .
cmake -DCMAKE_INSTALL_PREFIX:PATH="../../install"
make
make install
cd ..

