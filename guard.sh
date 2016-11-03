#!/bin/sh
while true; do
  clear;
  LD_LIBRARY_PATH=$PWD/aiscm/.libs guile -L $PWD -L $PWD/tests --no-auto-compile t.scm;
  inotifywait -e CLOSE_WRITE t.scm;
done
