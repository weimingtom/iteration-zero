#! /bin/sh

REV=-r1149

mkdir -p guichan
svn checkout $REV http://guichan.googlecode.com/svn/trunk/src/ guichan/src
svn checkout $REV http://guichan.googlecode.com/svn/trunk/include/ guichan/include
svn checkout $REV http://guichan.googlecode.com/svn/trunk/CMake/ guichan/CMake
svn cat $REV http://guichan.googlecode.com/svn/trunk/CMakeLists.txt > guichan/CMakeLists.in
sed -e 's/PROJECT(guichan)/PROJECT(guichan_x)/' \
  < guichan/CMakeLists.in > guichan/CMakeLists.txt



