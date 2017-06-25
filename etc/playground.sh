#!/bin/sh
while true; do
  clear;
  LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$PWD/../aiscm/.libs guile -L .. -L ../tests --no-auto-compile playground.scm;
  cat playground.log;
  inotifywait -e CLOSE_WRITE playground.scm;
done
