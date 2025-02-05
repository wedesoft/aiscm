#!/bin/sh
git log > ChangeLog
# mkdir -p build-aux
# touch build-aux/config.rpath
aclocal -I m4
autoheader
libtoolize --force
automake -a --foreign
autoconf
