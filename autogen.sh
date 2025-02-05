#!/bin/sh
git log > ChangeLog
aclocal -I m4
autoheader
libtoolize --force
automake -a --foreign
autoconf
