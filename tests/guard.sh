#!/bin/sh
while true; do
  clear;
  rm -f *.log;
  make $@ | grep FAIL;
  inotifywait -e CLOSE_WRITE `git ls-files ..`;
done
